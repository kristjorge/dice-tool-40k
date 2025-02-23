namespace DiceTool40K.Core

open DiceTool40K.Core.Domain
open DiceTool40K.Core.Rolls

module Damage =
    let fromInflictedDamage
        (fModDamage: ModifyDamageValue option)
        (baseDamage: DamageType)
        (damages: InflictedDamageType list)
        =
        let resolveDamage () =
            match fModDamage with
            | Some f -> DamageType.resolve baseDamage |> f
            | None -> DamageType.resolve baseDamage

        let damageTypeToDamage (damageType: InflictedDamageType) =
            match damageType with
            | InflictedDamageType.Normal -> [ resolveDamage () ]
            | InflictedDamageType.MortalWounds -> List.replicate (Damage.toInt (resolveDamage ())) (Damage 1)


        let rec looper (acc: Damage list) (remainder: InflictedDamageType list) =
            match remainder with
            | [] -> acc |> List.rev
            | dmgType :: rest -> looper ((damageTypeToDamage dmgType) @ acc) rest

        looper [] damages

module Resolve =

    type CombatResolution =
        { Damages: Damage list
          RemovedModels: RemovedModels }

    let numOfSimulations = 100000

    let removeModels (feelNoPain: FeelNoPain option) (woundsPerModel: Wounds) (damages: Damage list) =

        let applyFeelNoPain (damage: Damage) =
            match feelNoPain with
            | Some fnp -> FeelNoPain.apply fnp damage
            | None -> damage

        let rec looper (removedModels: RemovedModels) (remainingDamages: Damage list) (remainingWounds: Wounds) =
            match remainingDamages with
            | [] -> removedModels
            | dmg :: restOfDamage ->
                let dmg = applyFeelNoPain dmg
                let remainingWounds = Wounds.removeWounds remainingWounds dmg

                if (Wounds.toInt remainingWounds) = 0 then
                    looper (RemovedModels.removeModel removedModels) restOfDamage woundsPerModel
                else
                    looper removedModels restOfDamage remainingWounds

        looper RemovedModels.zero damages woundsPerModel

    let runSequence (defendingModels: DefendingModels) (attackingModels: AttackingModel list) =

        attackingModels
        |> List.collect (fun attackingModel ->

            let fReRollWound = ReRoll.createReRollFunction attackingModel.ReRollWound
            let fReRollHit = ReRoll.createReRollFunction attackingModel.ReRollHit
            let fCritHit = CriticalRoll.createCritFunction attackingModel.CritHit
            let fCritWound = CriticalRoll.createCritFunction attackingModel.CritWound

            let aggregatedHitModifier =
                DiceValueModifier.aggregate [ defendingModels.RollModifiers.ToHitModifier
                                              attackingModel.RollModifiers.ToHitModifier ]

            let aggregatedWoundModifier =
                DiceValueModifier.aggregate [ defendingModels.RollModifiers.ToWoundModifier
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
                       (Sequence.determineNumberOfWounds hitRoller woundRoller saveRoller hitRoller) |]
                |> List.ofArray
                |> List.collect id
                |> List.choose id
                |> List.sortBy (fun d ->
                    // Sort the wounds such that normal wounds are allocated first and then the mortals afterwards
                    match d with
                    | InflictedDamageType.Normal -> 0
                    | InflictedDamageType.MortalWounds -> 1)

            Damage.fromInflictedDamage defendingModels.DamageModifier attackingModel.WeaponProfile.Damage numWounds)

    let resolveCombat (defendingModels: DefendingModels) (attackingModels: AttackingModel list) =
        let damages =
            [| for i = 0 to numOfSimulations do
                   (runSequence defendingModels attackingModels) |]
            |> List.ofArray

        let removedModels =
            damages
            |> List.map (fun damages -> removeModels defendingModels.FeelNoPain defendingModels.Wounds damages)

        let avgRemovedModels =
            removedModels
            |> List.map (fun rm -> rm |> RemovedModels.toInt)
            |> List.map (fun rm -> rm |> double)
            |> List.average

        damages, removedModels, avgRemovedModels
