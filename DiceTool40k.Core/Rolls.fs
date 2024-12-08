namespace DiceTool40K.Core

open DiceTool40K.Core.Domain

module Attacks =
    let resolve (attacks: Attacks) =
        attacks
        |> function
            | Attacks.Constant n -> n
            | Attacks.Variable d -> Dice.rollDice d () |> DiceValue.toInt
            | Attacks.VariableModified (d, modifier) ->
                DiceValueModifier.modifyDiceRoll (Dice.rollDice d ()) modifier
                |> DiceValue.toInt

module SustainedHits =
    let resolve (sustainedHits: SustainedHits) =
        match sustainedHits with
        | SustainedHits.Constant n -> DiceValue n |> DiceValue.toInt
        | SustainedHits.Variable d -> Dice.rollDice d () |> DiceValue.toInt
        | SustainedHits.VariableModified (d, modifier) ->
            DiceValueModifier.modifyDiceRoll (Dice.rollDice d ()) modifier
            |> DiceValue.toInt

module DamageType =
    let resolve (dmgType: DamageType) =
        match dmgType with
        | DamageType.Constant n -> Damage n
        | DamageType.Variable dice -> Dice.rollDice dice () |> DiceValue.toInt |> Damage
        | DamageType.VariableModified (d, modifier) ->
            DiceValueModifier.modifyDiceRoll (Dice.rollDice d ()) modifier
            |> DiceValue.toInt
            |> Damage

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

        let toSave (save: Save) (invul: InvulSave option) (ap: ArmourPiercing) =
            let modSave = Save.applyArmourPiercing save ap

            match invul with
            | Some (InvulSave (RequiredDiceRoll i)) ->
                if (Save.toInt modSave) >= i then
                    RequiredDiceRoll i
                else
                    RequiredDiceRoll(Save.toInt modSave)
            | None -> RequiredDiceRoll(Save.toInt modSave)


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
            (modifier: DiceValueModifier)
            (rolledDiceValue: DiceValue)
            =
            let modifiedRoll = DiceValueModifier.modifyDiceRoll rolledDiceValue modifier
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
            (rollModifier: DiceValueModifier)
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
            (woundModifier: DiceValueModifier)
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
                        [ ActionSet.Damage InflictedDamageType.Normal ] // Dev wounds does not yield mortal wounds anymore
                        |> Some
                    else
                        [ ActionSet.Saving ] |> Some


        let rollToSave (save: Save) (invul: InvulSave option) (ap: ArmourPiercing) (diceRoller: unit -> DiceValue) =
            let requiredSaveRoll = RequiredDiceRoll.toSave save invul ap

            diceRoller ()
            |> successfulRoll None requiredSaveRoll DiceValueModifier.zero // Can not modify save value
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



    module FeelNoPain =
        let apply (feelNoPain: FeelNoPain) (damage: Damage) =
            // Takes FeeplNoPain and Damage and rolls the FeelNoPain roll. For each successfull roll (Some DiceValue)
            // the total number of damage is subtracted by 1c
            let numDamage = Damage.toInt damage

            let tryApply =
                (Dice.rollDice Dice.D6 ()
                 |> Dice.successfulRoll None (FeelNoPain.toRequiredRoll feelNoPain) DiceValueModifier.noModifier)

            List.init numDamage (fun _ -> tryApply)
            |> List.choose id
            |> List.length
            |> fun mitiagedDamage -> numDamage - mitiagedDamage
            |> Damage
