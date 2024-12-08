namespace DiceTool40K.Core

open System
open DiceTool40K.Core.ProgramError

module Domain =

    let rnd = System.Random()

    type DiceValue = DiceValue of int
    type DiceValueModified = DiceValueModified of int
    type DiceValueModifier = DiceValueModifier of int

    module DiceValueModified =
        let toInt (DiceValueModified modifiedValue) = modifiedValue

    type Dice =
        | D6
        | D3

    module Dice =

        let rollDice (diceType: Dice) () =
            match diceType with
            | D6 -> rnd.Next(1, 7) |> int |> DiceValue
            | D3 -> rnd.Next(1, 4) |> int |> DiceValue

    module DiceValue =

        let six = DiceValue 6

        let create (value: int) =
            if value > 6 || value < 1 then
                Error(ProgramError.InvalidDiceValue value)
            else
                DiceValue value |> Ok

        let toInt (DiceValue dice) = dice

    module DiceValueModifier =

        let noModifier = DiceValueModifier 0

        let zero = DiceValueModifier 0

        let create (value: int) = DiceValueModifier value

        let toInt (DiceValueModifier modifierValue) = modifierValue

        let aggregate (modifiers: DiceValueModifier list) =
            modifiers
            |> List.map toInt
            |> List.reduce (fun mod1 mod2 -> mod1 + mod2)
            |> fun modifier ->
                if modifier < -1 then -1
                elif modifier > 1 then 1
                else modifier
            |> DiceValueModifier

        let modifyDiceRoll (diceValue: DiceValue) (modifier: DiceValueModifier) =
            DiceValue.toInt diceValue
            |> fun d -> d + (toInt modifier)
            |> DiceValue


    type RequiredDiceRoll = RequiredDiceRoll of int

    module RequiredDiceRoll =

        let toInt (RequiredDiceRoll required) = required

        let create (value: int) =
            if value > 1 then
                RequiredDiceRoll value |> Ok
            else
                Error(ProgramError.InvalidRequiredDiceRoll value)

        let compare (roll: DiceValue) (required: RequiredDiceRoll) =
            (DiceValue.toInt roll) >= (toInt required)

    type ReRoll =
        | Ones
        | Full

    type DiceRollOutcome =
        | Ordinary of DiceValue
        | Critial of DiceValue

    type Skill = Skill of int
    type Strength = Strength of int
    type Toughness = Toughness of int
    type InvulSave = InvulSave of RequiredDiceRoll
    type Save = Save of RequiredDiceRoll
    type FeelNoPain = FeelNoPain of RequiredDiceRoll

    type ArmourPiercing = ArmourPiercing of int

    module ArmourPiercing =

        let toInt (ArmourPiercing ap) = ap

        let create (value: int) =
            if value >= 0 then
                ArmourPiercing value |> Ok
            else
                (ProgramError.InvalidArmourPiercingValue value)
                |> Error

    module Save =

        let toInt (Save (RequiredDiceRoll save)) = save

        let create (value: int) =
            match (RequiredDiceRoll.create value) with
            | Ok req -> Save req |> Ok
            | Error _ -> ProgramError.InvalidSaveValue value |> Error

        let applyArmourPiercing (save: Save) (armourPiercing: ArmourPiercing) =
            // ArmourPiercing is always positive, meaning a RequiredDiceRoll modified
            // by a positive ArmoudPiercing is also itself always positive
            // Does not need Result<RequiredDiceRoll, ProgramError> because of that
            let modSave =
                (toInt save)
                + (ArmourPiercing.toInt armourPiercing)

            Save(RequiredDiceRoll modSave)


    module InvulSave =

        let toInt (InvulSave (RequiredDiceRoll invul)) = invul

        let create (value: int) =
            match (RequiredDiceRoll.create value) with
            | Ok req -> InvulSave req |> Ok
            | Error _ -> ProgramError.InvalidInvulSaveValue value |> Error


    type Attacks =
        | Constant of int
        | Variable of Dice
        | VariableModified of (Dice * DiceValueModifier)

    type SustainedHits =
        | Constant of int
        | Variable of Dice
        | VariableModified of (Dice * DiceValueModifier)

    type DamageType =
        | Constant of int
        | Variable of Dice
        | VariableModified of (Dice * DiceValueModifier)

    type Damage = Damage of int

    module FeelNoPain =
        let create (value: int) =
            match RequiredDiceRoll.create value with
            | Ok req -> FeelNoPain req |> Ok
            | Error _ -> ProgramError.InvalidFeelNoPainValue value |> Error

        let toInt (FeelNoPain feelNoPain) = feelNoPain

        let toRequiredRoll (FeelNoPain feelNoPain) = feelNoPain


    [<RequireQualifiedAccess>]
    type InflictedDamageType =
        | Normal
        | MortalWounds

    type ModifyDamageValue = Damage -> Damage

    module Damage =

        let halfDamage (Damage damage) =
            System.Math.Ceiling(((float damage) / (float 2)))
            |> int
            |> Damage

        let minusOne (Damage damage) = (damage - 1) |> max 1 |> Damage

        let toInt (Damage dmg) = dmg

    type Wounds = Wounds of int

    module Wounds =

        let toInt (Wounds wounds) = wounds

        let create (value: int) =
            if value > 0 then
                Wounds value |> Ok
            else
                ProgramError.WoundsMustBePositive value |> Error

        let removeWounds (remainingWounds: Wounds) (damage: Damage) =
            (remainingWounds |> toInt)
            - (damage |> Damage.toInt)
            |> max 0
            |> Wounds


    module Strength =
        let create (value: int) =
            if value > 0 then
                Strength value |> Ok
            else
                (ProgramError.StrengthMustBePositive value)
                |> Error

    module Toughness =
        let create (value: int) =
            if value > 0 then
                Toughness value |> Ok
            else
                ProgramError.ToughnessMustBePositive value
                |> Error

    module Skill =
        let create (value: int) =
            if value > 1 && value < 7 then
                Skill value |> Ok
            else
                ProgramError.InvalidSkillValue value |> Error

    type NumModels = NumModels of int

    module NumModels =

        let toInt (NumModels numModels) = numModels

        let create (value: int) =
            if value > 0 then
                NumModels value |> Ok
            else
                ProgramError.NumModelsMostBePositive value
                |> Error

    type RemovedModels = RemovedModels of int

    module RemovedModels =

        let zero = 0 |> RemovedModels
        let toInt (RemovedModels num) = num

        let removeModel (removedModels: RemovedModels) =
            removedModels
            |> toInt
            |> fun x -> x + 1
            |> RemovedModels

    type RollModifiers =
        { ToHitModifier: DiceValueModifier
          ToWoundModifier: DiceValueModifier }

    type DefendingModels =
        { Toughness: Toughness
          Save: Save
          Invul: InvulSave option
          Wounds: Wounds
          DamageModifier: ModifyDamageValue option
          RollModifiers: RollModifiers
          FeelNoPain: FeelNoPain option
          NumModels: NumModels }

    type WeaponSpecialRules =
        { LethalHits: bool
          SustainedHits: SustainedHits option
          DevestatingWounds: bool
          Blast: bool }

    module WeaponSpecialRules =
        module Blast =
            let applyBlast (baseAttacks: int) (numModels: NumModels) =
                baseAttacks + (NumModels.toInt numModels) / 5


    type WeaponProfile =
        { Skill: Skill
          Attacks: Attacks
          Strength: Strength
          ArmourPiercing: ArmourPiercing
          Damage: DamageType }

    type AttackingModel =
        { WeaponProfile: WeaponProfile
          SpecialRules: WeaponSpecialRules
          RollModifiers: RollModifiers
          ReRollHit: ReRoll option
          ReRollWound: ReRoll option
          CritHit: DiceValue option
          CritWound: DiceValue option }


    [<RequireQualifiedAccess>]
    type ActionSet =
        | Hitting
        | Wounding
        | Saving
        | Damage of InflictedDamageType
