namespace DiceTool40K.Core

open DiceTool40K.Core.Domain
open DiceTool40K.Core.Rolls

module Resolve =

    let numOfSimulations = 50000

    let removeModels (woundsPerModel: Wounds) (damages: Damage list) =

        let rec looper (removedModels: RemovedModels) (remainingDamages: Damage list) (remainingWounds: Wounds) =
            match remainingDamages with
            | [] -> removedModels
            | dmg :: restOfDamage ->
                let remainingWounds = Wounds.removeWounds remainingWounds dmg

                if (Wounds.toInt remainingWounds) = 0 then
                    looper (RemovedModels.removeModel removedModels) restOfDamage woundsPerModel
                else
                    looper removedModels restOfDamage remainingWounds

        looper RemovedModels.zero damages woundsPerModel

    let resolveCombat (defendingModels: DefendingModels) (attackingModels: AttackingModel list) =
        let damages =
            [| for i = 0 to numOfSimulations do
                   (Dice.runSequence defendingModels attackingModels) |]
            |> List.ofArray

        let removedModels =
            damages
            |> List.map (fun damages -> removeModels defendingModels.Wounds damages)


        let avgRemovedModels =
            removedModels
            |> List.map (fun rm -> rm |> RemovedModels.toInt)
            |> List.map (fun rm -> rm |> double)
            |> List.average

        damages, removedModels, avgRemovedModels
