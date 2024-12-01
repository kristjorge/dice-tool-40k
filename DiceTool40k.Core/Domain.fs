namespace DiceTool40K.Core

open System
open DiceTool40K.Core.ProgramError

module Domain =

    let rnd = System.Random()

    type DiceValue = DiceValue of int
    type DiceValueModified = DiceValueModified of int

    module DiceValueModified =
        let toInt (DiceValueModified modifiedValue) = modifiedValue

    type DiceModifier = DiceModifier of int

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

    type Dice =
        | D6
        | D3

    module Dice =

        let rollDice (diceType: Dice) () =
            match diceType with
            | D6 -> rnd.Next(1, 7) |> int |> DiceValue
            | D3 -> rnd.Next(1, 4) |> int |> DiceValue



    module DiceModifier =

        let noModifier = DiceModifier 0

        let create (value: int) = DiceModifier value

        let toInt (DiceModifier modifierValue) = modifierValue

        let aggregate (modifiers: DiceModifier list) =
            modifiers
            |> List.map toInt
            |> List.reduce (fun x y -> x + y)
            |> fun modifier ->
                if modifier < -1 then -1
                elif modifier > 1 then 1
                else modifier
            |> DiceModifier

        let modifyDiceRoll (diceValue: DiceValue) (modifier: DiceModifier) =
            DiceValue.toInt diceValue
            |> fun d -> d + (toInt modifier)
            |> DiceValue



    type RequiredDiceRoll = RequiredDiceRoll of int

    module RequiredDiceRoll =

        let create (DiceValue value) = RequiredDiceRoll value

        let compare (DiceValue roll) (RequiredDiceRoll required) = roll >= required

    type ReRoll =
        | Ones
        | Full

    type DiceRollOutcome =
        | Ordinary of DiceValue
        | Critial of DiceValue

    type Skill = Skill of int
    type Strength = Strength of int


    type Toughness = Toughness of int
    type InvulSave = InvulSave of int
    type Save = Save of int
    type FeelNoPain = FeelNoPain of int

    module Save =
        let create (value: int) =
            if value > 1 then
                Save value |> Ok
            else
                ProgramError.InvalidSaveValue value |> Error


    module InvulSave =
        let create (value: int) =
            if value > 1 then
                InvulSave value |> Ok
            else
                ProgramError.InvalidInvulSaveValue value |> Error




    type Attacks =
        | Constant of int
        | Variable of Dice
        | VariableModified of (Dice * DiceModifier)

    module Attacks =
        let resolve (attacks: Attacks) =
            attacks
            |> function
                | Attacks.Constant n -> n
                | Attacks.Variable d -> Dice.rollDice d () |> DiceValue.toInt
                | Attacks.VariableModified (d, modifier) ->
                    DiceModifier.modifyDiceRoll (Dice.rollDice d ()) modifier
                    |> DiceValue.toInt

    type SustainedHits =
        | Constant of int
        | Variable of Dice
        | VariableModified of (Dice * DiceModifier)

    module SustainedHits =
        let resolve (sustainedHits: SustainedHits) =
            match sustainedHits with
            | Constant n -> DiceValue n |> DiceValue.toInt
            | Variable d -> Dice.rollDice d () |> DiceValue.toInt
            | VariableModified (d, modifier) ->
                DiceModifier.modifyDiceRoll (Dice.rollDice d ()) modifier
                |> DiceValue.toInt


    type ArmourPiercing = ArmourPiercing of int

    module ArmourPiercing =
        let create (value: int) =
            if value >= 0 then
                ArmourPiercing value |> Ok
            else
                (ProgramError.InvalidArmourPiercingValue value)
                |> Error

    type DamageType =
        | Constant of int
        | Variable of Dice
        | VariableModified of (Dice * DiceModifier)

    type Damage = Damage of int

    module DamageType =
        let resolve (dmgType: DamageType) =
            match dmgType with
            | DamageType.Constant n -> Damage n
            | DamageType.Variable dice -> Dice.rollDice dice () |> DiceValue.toInt |> Damage
            | DamageType.VariableModified (d, modifier) ->
                DiceModifier.modifyDiceRoll (Dice.rollDice d ()) modifier
                |> DiceValue.toInt
                |> Damage


    module FeelNoPain =
        let create (value: int) =
            match DiceValue.create value with
            | Ok dv -> FeelNoPain(DiceValue.toInt dv) |> Ok
            | Error _ -> ProgramError.InvalidFeelNoPainValue value |> Error

        let toInt (FeelNoPain feelNoPain) = feelNoPain

        let toRequiredRoll (feelNoPain: FeelNoPain) = feelNoPain |> toInt |> RequiredDiceRoll


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
                | InflictedDamageType.MortalWounds -> List.replicate (toInt (resolveDamage ())) (Damage 1)


            let rec looper (acc: Damage list) (remainder: InflictedDamageType list) =
                match remainder with
                | [] -> acc |> List.rev
                | dmgType :: rest -> looper ((damageTypeToDamage dmgType) @ acc) rest

            looper [] damages

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
        { ToHitModifier: DiceModifier
          ToWoundModifier: DiceModifier }

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
