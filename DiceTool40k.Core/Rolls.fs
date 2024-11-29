namespace DiceTool40K.Core

open DiceTool40K.Core.Domain

module Rolls =

    module RequiredDiceRoll =
        let toHit (Skill skill) = RequiredDiceRoll skill

        let toWound (Strength S) (Toughness T) =
            let ratio = (double S) / (double T)

            if ratio = 2.0 then
                RequiredDiceRoll 2
            elif ratio = 0.5 then
                RequiredDiceRoll 6
            elif ratio > 1.0 && ratio < 2.0 then
                RequiredDiceRoll 3
            elif ratio < 1.0 && ratio < 0.5 then
                RequiredDiceRoll 5
            else
                RequiredDiceRoll 4

        let toSave (Save save) (invul: InvulSave option) (ArmourPiercing ap) =
            let modSave = save + ap

            match invul with
            | Some (InvulSave invulValue) ->
                if modSave >= invulValue then
                    RequiredDiceRoll invulValue
                else
                    RequiredDiceRoll modSave
            | None -> RequiredDiceRoll modSave


    type CriticalRollCheck = DiceValue -> bool

    type RerollCheck = RequiredDiceRoll -> DiceValue -> bool


    module Dice =

        let fReRoll (reRoll: ReRoll) (requiredDiceRoll: RequiredDiceRoll) =
            match reRoll with
            | Ones ->
                let checker (diceRoll: DiceValue) =
                    (RequiredDiceRoll.compare diceRoll requiredDiceRoll)
                    && (diceRoll |> DiceValue.toInt) = 1

                checker
            | Full ->
                let checker (diceRoll: DiceValue) =
                    RequiredDiceRoll.compare diceRoll requiredDiceRoll

                checker

        module CriticalRoll =

            let fCriticalRoll (critValue: DiceValue) : CriticalRollCheck = fun x -> (x >= critValue)

            let Default = fCriticalRoll (DiceValue 6)

        let rec successfulRoll
            (fReRollCheck: RerollCheck option)
            (required: RequiredDiceRoll)
            (modifier: DiceModifier)
            (rolledDiceValue: DiceValue)
            =
            let modifiedRoll = DiceModifier.modifyDiceRoll rolledDiceValue modifier
            let isSuccessful = RequiredDiceRoll.compare modifiedRoll required

            match isSuccessful, fReRollCheck with
            | true, _ -> rolledDiceValue |> Some
            | false, None -> None
            | false, Some _ -> successfulRoll None required modifier (Dice.rollDice (Dice.D6) ())


        let criticalRoll (fCrit: CriticalRollCheck) (diceRoll: DiceValue) =

            let isCrit = fCrit diceRoll

            match isCrit with
            | true -> DiceRollOutcome.Critial diceRoll |> Some
            | false -> DiceRollOutcome.Ordinary diceRoll |> Some

        let rollToHit
            (fCrit: CriticalRollCheck)
            (fReRoll: RerollCheck option)
            (rollModifier: DiceModifier)
            (lethalHit: bool)
            (sustainedHits: SustainedHits option)
            (skill: Skill)
            (diceRoller: unit -> DiceValue)
            =

            let numSustainedHits (sustainedHits: SustainedHits) = SustainedHits.resolve sustainedHits

            diceRoller ()
            |> successfulRoll fReRoll (RequiredDiceRoll.toHit skill) rollModifier
            |> Option.bind (criticalRoll fCrit)
            |> function
                | None -> None // Miss
                | Some (Ordinary _) -> [ ActionSet.Wounding ] |> Some
                | Some (DiceRollOutcome.Critial _) ->
                    match (lethalHit, sustainedHits) with
                    | true, Some sustainedHits ->

                        [ ActionSet.Saving ]
                        @ List.init (numSustainedHits sustainedHits) (fun _ -> ActionSet.Wounding)
                        |> Some
                    | true, None -> [ ActionSet.Saving ] |> Some
                    | false, None -> [ ActionSet.Wounding ] |> Some
                    | false, Some sustainedHits ->
                        [ ActionSet.Wounding ]
                        @ List.init (numSustainedHits sustainedHits) (fun _ -> ActionSet.Wounding)
                        |> Some

        let rollToWound
            (fCrit: CriticalRollCheck)
            (fReRoll: RerollCheck option)
            (woundModifier: DiceModifier)
            (devestatingWounds: bool)
            (toughness: Toughness)
            (strength: Strength)
            (diceRoller: unit -> DiceValue)
            =

            diceRoller ()
            |> successfulRoll fReRoll (RequiredDiceRoll.toWound strength toughness) woundModifier
            |> Option.bind (criticalRoll fCrit)
            |> function
                | None -> None
                | Some (Ordinary _) -> [ ActionSet.Saving ] |> Some
                | Some (Critial _) ->
                    if devestatingWounds then
                        [ ActionSet.Damage InflictedDamageType.MortalWounds ]
                        |> Some
                    else
                        [ ActionSet.Saving ] |> Some


        let rollToSave (save: Save) (invul: InvulSave option) (ap: ArmourPiercing) (diceRoller: unit -> DiceValue) =
            diceRoller ()
            |> successfulRoll None (RequiredDiceRoll.toSave save invul ap) (DiceModifier 0) // Can not modify save value
            |> function
                | None ->
                    [ ActionSet.Damage InflictedDamageType.Normal ]
                    |> Some // Failing saving throw means damage
                | Some _ -> None


        let rec determineNumberOfWounds
            (hitRoller: (unit -> DiceValue) -> ActionSet list option)
            (woundRoller: (unit -> DiceValue) -> ActionSet list option)
            (saveRoller: (unit -> DiceValue) -> ActionSet list option)
            (currentRoller: (unit -> DiceValue) -> ActionSet list option)
            =

            match currentRoller (Dice.rollDice Dice.D6) with
            | None -> [ None ]
            | Some actionSet ->
                actionSet
                |> List.collect (fun action ->
                    match action with
                    | ActionSet.Hitting -> determineNumberOfWounds hitRoller woundRoller saveRoller hitRoller
                    | ActionSet.Wounding -> determineNumberOfWounds hitRoller woundRoller saveRoller woundRoller
                    | ActionSet.Saving -> determineNumberOfWounds hitRoller woundRoller saveRoller saveRoller
                    | ActionSet.Damage damageType -> [ (Some damageType) ])

        let runSequence (defendingModels: DefendingModels) (attackingModels: AttackingModel list) =

            let createReRollFunction (reRoll: ReRoll option) =
                match reRoll with
                | None -> None
                | Some rr -> fReRoll rr |> Some

            let createCritFunction (critValue: DiceValue option) =
                match critValue with
                | None -> CriticalRoll.fCriticalRoll (DiceValue.six)
                | Some value -> CriticalRoll.fCriticalRoll value


            attackingModels
            |> List.collect (fun attackingModel ->

                let fReRollWound = createReRollFunction attackingModel.ReRollWound
                let fReRollHit = createReRollFunction attackingModel.ReRollHit
                let fCritHit = createCritFunction attackingModel.CritHit
                let fCritWound = createCritFunction attackingModel.CritWound

                let aggregatedHitModifier =
                    DiceModifier.aggregate [ defendingModels.RollModifiers.ToHitModifier
                                             attackingModel.RollModifiers.ToHitModifier ]

                let aggregatedWoundModifier =
                    DiceModifier.aggregate [ defendingModels.RollModifiers.ToWoundModifier
                                             attackingModel.RollModifiers.ToWoundModifier ]


                let hitRoller =
                    rollToHit
                        fCritHit
                        fReRollHit
                        aggregatedHitModifier
                        attackingModel.SpecialRules.LethalHits
                        attackingModel.SpecialRules.SustainedHits
                        attackingModel.WeaponProfile.Skill

                let woundRoller =
                    rollToWound
                        fCritWound
                        fReRollWound
                        aggregatedWoundModifier
                        attackingModel.SpecialRules.DevestatingWounds
                        defendingModels.Toughness
                        attackingModel.WeaponProfile.Strength

                let saveRoller =
                    rollToSave defendingModels.Save defendingModels.Invul attackingModel.WeaponProfile.ArmourPiercing

                let numAttacks = Attacks.resolve attackingModel.WeaponProfile.Attacks

                let numAttacks =
                    match attackingModel.SpecialRules.Blast with
                    | false -> numAttacks
                    | true -> WeaponSpecialRules.Blast.applyBlast numAttacks defendingModels.NumModels

                let numWounds =
                    [| for i = 0 to (numAttacks - 1) do
                           (determineNumberOfWounds hitRoller woundRoller saveRoller hitRoller) |]
                    |> List.ofArray
                    |> List.collect id
                    |> List.choose id
                    |> List.sortBy (fun d ->
                        // Sort the wounds such that normal wounds are allocated first and then the mortals afterwards
                        match d with
                        | InflictedDamageType.Normal -> 0
                        | InflictedDamageType.MortalWounds -> 1)

                Damage.fromInflictedDamage defendingModels.DamageModifier attackingModel.WeaponProfile.Damage numWounds)
