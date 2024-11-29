namespace DiceTool40K.Core


open System
open System.Text.Json

open FsToolkit.ErrorHandling
open DiceTool40K.Core.Contract
open DiceTool40K.Core.Domain


module Config =

    let deserializeJson<'T> (json: string) : 'T = JsonSerializer.Deserialize<'T>(json)

    let (|IgnoreCaseEqual|_|) (str: string) arg =
        if String.Compare(str, arg, StringComparison.OrdinalIgnoreCase) = 0 then
            Some()
        else
            None

    module ReRoll =
        let toDomain (value: string) =
            match value with
            | IgnoreCaseEqual "ones" -> ReRoll.Ones |> Ok
            | IgnoreCaseEqual "full" -> ReRoll.Full |> Ok
            | _ -> Error(ProgramError.UnableToParseReRollTypeDto value)

    module DiceType =
        let toDomain (value) =
            match value with
            | IgnoreCaseEqual "d6" -> Dice.D6 |> Ok
            | IgnoreCaseEqual "d3" -> Dice.D3 |> Ok
            | x -> Error(ProgramError.UnableToParseDiceTypeDto x)

    module DamageModifier =
        let toDomain (value: string) =
            match value with
            | IgnoreCaseEqual "damageMinusOne" -> Damage.minusOne |> Ok
            | IgnoreCaseEqual "halfDamage" -> Damage.halfDamage |> Ok
            | x -> Error(ProgramError.UnableToParseDamageModifierType x)


    module DiceModifier =
        let toDomainFromOption (value: int option) =
            match value with
            | None -> DiceModifier.create 0
            | Some v -> DiceModifier.create v

    module SustainedHitsDto =

        let toDomain (dto: SustainedHitsDto) =
            let constant = dto.Constant
            let variable = dto.Variable

            match variable with
            | None ->
                if constant > 0 then
                    SustainedHits.Constant constant |> Ok
                else
                    Error(ProgramError.NumConstantSustainedHitsMustBePositive constant)

            | Some d ->

                result {
                    let! varSustained = DiceType.toDomain d

                    if constant <> 0 then
                        return (SustainedHits.VariableModified(varSustained, DiceModifier constant))
                    else
                        return (SustainedHits.Variable varSustained)
                }

    module AttacksDto =
        let toDomain (dto: AttacksDto) =
            let constant = dto.Constant
            let variable = dto.Variable

            match variable with
            | None ->
                if constant > 0 then
                    Attacks.Constant constant |> Ok
                else
                    Error(ProgramError.NumConstantSustainedHitsMustBePositive constant)

            | Some d ->

                result {
                    let! varAttacks = DiceType.toDomain d

                    if constant <> 0 then
                        return (Attacks.VariableModified(varAttacks, DiceModifier constant))
                    else
                        return (Attacks.Variable varAttacks)
                }

    module DamageTypeDto =
        let toDomain (dto: DamageTypeDto) =
            let constant = dto.Constant
            let variable = dto.Variable

            match variable with
            | None ->
                if constant > 0 then
                    DamageType.Constant constant |> Ok
                else
                    Error(ProgramError.NumConstantSustainedHitsMustBePositive constant)

            | Some d ->

                result {
                    let! varDamage = DiceType.toDomain d

                    if constant <> 0 then
                        return (DamageType.VariableModified(varDamage, DiceModifier constant))
                    else
                        return (DamageType.Variable varDamage)
                }

    module SpecialRulesDto =
        let toDomain (dto: SpecialRulesDto) =
            result {
                let! sustainedHits = SustainedHitsDto.toDomain dto.SustainedHits

                return
                    { WeaponSpecialRules.LethalHits = dto.LetalHits
                      DevestatingWounds = dto.DevestatingWounds
                      Blast = dto.Blast
                      SustainedHits = sustainedHits |> Some }
            }


    module AttackingModelDto =
        let toDomain (dto: AttackingModelDto) =
            result {
                let! numAttacks = AttacksDto.toDomain (dto.NumAttacks)
                let! armourPiercing = ArmourPiercing.create dto.ArmourPiercing
                let! strength = Strength.create dto.Strength
                let! damage = DamageTypeDto.toDomain dto.Damage
                let! specialRules = SpecialRulesDto.toDomain dto.SpecialRules
                let! skill = Skill.create dto.Skill

                let makeReRollFromOption (reRoll: string option) =
                    match reRoll with
                    | None -> None |> Ok
                    | Some r ->
                        match ReRoll.toDomain r with
                        | Ok x -> Ok(Some x)
                        | Error error -> Error error


                let makeDiceValueFromOption (value: int option) =
                    match value with
                    | None -> None |> Ok
                    | Some v ->
                        match DiceValue.create v with
                        | Error e -> Error e
                        | Ok diceValue -> Ok(Some diceValue)




                let toHitModifier = DiceModifier.toDomainFromOption dto.ToHitModifier
                let toWoundModifier = DiceModifier.toDomainFromOption dto.ToWoundModifier
                let! toCritHit = makeDiceValueFromOption dto.CritHitDiceValue
                let! toCritWound = makeDiceValueFromOption dto.CritWoundDiceValue
                let! reRollHit = makeReRollFromOption dto.ReRollHit
                let! reRollWound = makeReRollFromOption dto.ReRollWounds


                let weaponProfile =
                    { WeaponProfile.Attacks = numAttacks
                      Skill = skill
                      Strength = strength
                      ArmourPiercing = armourPiercing
                      Damage = damage }

                return
                    { WeaponProfile = weaponProfile
                      SpecialRules = specialRules
                      RollModifiers =
                        { ToHitModifier = toHitModifier
                          ToWoundModifier = toWoundModifier }
                      ReRollHit = reRollHit
                      ReRollWound = reRollWound
                      CritHit = toCritHit
                      CritWound = toCritWound }
            }

    module DefendingModelDto =
        let toDomain (dto: DefendingModelsDto) =
            result {
                let! toughness = Toughness.create dto.Toughness
                let! save = Save.create dto.Save

                let! invul =
                    match dto.Invul with
                    | None -> None |> Ok
                    | Some invul ->
                        match InvulSave.create invul with
                        | Ok inv -> Some inv |> Ok
                        | Error e -> Error e


                let! wounds = Wounds.create dto.Wounds
                let! numModels = NumModels.create dto.NumModels

                let! dmgMod =
                    match dto.DamageModifier with
                    | None -> None |> Ok
                    | Some dmgMod ->
                        match (DamageModifier.toDomain dmgMod) with
                        | Ok dmgMod -> Some dmgMod |> Ok
                        | Error error -> Error error

                let hitModifier = DiceModifier.toDomainFromOption dto.ToHitModifier
                let toWoundModifier = DiceModifier.toDomainFromOption dto.ToWoundModifier

                return
                    { DefendingModels.Toughness = toughness
                      Save = save
                      Invul = invul
                      Wounds = wounds
                      DamageModifier = dmgMod
                      RollModifiers =
                        { ToHitModifier = hitModifier
                          ToWoundModifier = toWoundModifier }
                      NumModels = numModels }
            }
