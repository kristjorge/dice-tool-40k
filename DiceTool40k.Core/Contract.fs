namespace DiceTool40K.Core

open DiceTool40K.Core.Resolve
open DiceTool40K.Core.Domain

module Contract =
    type SustainedHitsDto =
        { Constant: int
          Variable: string option }

    type AttacksDto =
        { Constant: int
          Variable: string option }

    type SpecialRulesDto =
        { LetalHits: bool
          DevestatingWounds: bool
          SustainedHits: SustainedHitsDto
          Blast: bool }

    type DamageTypeDto =
        { Constant: int
          Variable: string option }

    type AttackingModelDto =
        { NumAttacks: AttacksDto
          Skill: int
          ArmourPiercing: int
          Strength: int
          Damage: DamageTypeDto
          SpecialRules: SpecialRulesDto
          ToHitModifier: int option
          ToWoundModifier: int option
          CritHitDiceValue: int option
          CritWoundDiceValue: int option
          ReRollHit: string option
          ReRollWounds: string option }

    type DefendingModelsDto =
        { Toughness: int
          Save: int
          Invul: int option
          Wounds: int
          DamageModifier: string option
          ToHitModifier: int option
          ToWoundModifier: int option
          FeelNoPain: int option
          NumModels: int }

    type ResolveCombatDto =
        { AttackingModels: AttackingModelDto list
          DefendingModels: DefendingModelsDto }

    type CombatResolutionDto =
        { Damages: int list
          RemovedModels: int }

    module CombatResolutionDto =
        let fromDomain (resolution: Resolve.CombatResolution) =
            { CombatResolutionDto.Damages = resolution.Damages |> List.map Damage.toInt
              RemovedModels = resolution.RemovedModels |> RemovedModels.toInt }
