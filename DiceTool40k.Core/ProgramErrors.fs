namespace DiceTool40K.Core

module ProgramError =

    type ProgramError =
        | StrengthMustBePositive of int
        | ToughnessMustBePositive of int
        | InvalidSkillValue of int
        | InvalidSaveValue of int
        | InvalidInvulSaveValue of int
        | NumModelsMostBePositive of int
        | DamageMustBePositive of int
        | InvalidArmourPiercingValue of int
        | WoundsMustBePositive of int
        | UnableToParseDiceTypeDto of string
        | UnableToParseReRollTypeDto of string
        | UnableToParseDamageModifierType of string
        | NumConstantSustainedHitsMustBePositive of int
        | NumConstantAttacksMustBePositive of int
        | NumConstantDamageMustBePositive of int
        | ConstantTermMustBePositive of int
        | InvalidDiceValue of int
        | InvalidFeelNoPainValue of int

    let parseErrors (error: ProgramError) =
        match error with
        | StrengthMustBePositive s -> $"Strength must be a positive value ({s} received)"
        | ToughnessMustBePositive t -> $"Toughness must be a positive value ({t} received)"
        | InvalidSkillValue s -> $"Skill value must be between 2 and 6 (inclusive) ({s} received)"
        | DamageMustBePositive d -> $"Damage value must be positive. ({d} received)"
        | InvalidArmourPiercingValue ap -> $"Armour piercing value must be 0 or greater ({ap} received)"
        | WoundsMustBePositive wounds -> $"Number of wounds must be positive ({wounds} received)"
        | UnableToParseDiceTypeDto value -> $"Unable to parse '{value}' as DiceType. 'D3' or 'D6' expected."
        | NumConstantSustainedHitsMustBePositive x ->
            $"Number of constant sustained hits must be larger than zero ({x})"
        | NumConstantAttacksMustBePositive x -> $"Number of constant sustained hits must be larger than zero ({x})"
        | NumConstantDamageMustBePositive x -> $"Number of constant damage hits must be larger than zero ({x})"
        | ConstantTermMustBePositive x -> $"Constant term must be positive ({x})"
        | InvalidSaveValue x -> $"Save value must be 2+ or worse ({x})"
        | InvalidInvulSaveValue x -> $"Invul ave value must be 2+ or worse ({x})"
        | NumModelsMostBePositive x -> $"Number of defending models must be positive ({x})"
        | UnableToParseDamageModifierType x -> $"Unable to parse damage modifier string ({x})"
        | InvalidDiceValue x -> $"Invalid DiceValue received ({x})"
        | UnableToParseReRollTypeDto x -> $"Unable to parse ReRoll type dto. Expected 'full' or 'ones', received '{x}'"
        | InvalidFeelNoPainValue x -> $"FeelNoPain value must be between 2 and 6 ('{x}' received)"
