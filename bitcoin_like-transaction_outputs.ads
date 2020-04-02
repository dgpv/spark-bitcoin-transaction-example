-- Copyright (C) 2019-2020 Dmitry Petukhov https://github.com/dgpv
--
-- This file is part of spark-bitcoin-transaction-example
--
-- It is subject to the license terms in the LICENSE file found in the top-level
-- directory of this distribution.
--
-- No part of spark-bitcoin-transaction-example, including this file, may be copied, modified,
-- propagated, or distributed except according to the terms contained in the
-- LICENSE file.

pragma SPARK_Mode(On);

with Data_Controls;
with Utils; use Utils;

with Bitcoin;

with Bitcoin_Like.Scripts;
with Bitcoin_Like.Data_Accessors;
with Bitcoin_Like.Transaction_Model;

with Bitcoin_Like.Utils;
use Bitcoin_Like.Utils;

generic
    with package Data_Readers is new Bitcoin_Like.Data_Accessors.Readers (<>);
    with package Data_Writers is new Bitcoin_Like.Data_Accessors.Writers (<>);
    with package Scripts is new Bitcoin_Like.Scripts (Data_Readers => Data_Readers,
                                                      Data_Writers => Data_Writers,
                                                      others => <>);
    type Satoshi_Type is range <>;
package Bitcoin_Like.Transaction_Outputs is

    use Bitcoin_Like.Transaction_Model;
    use Transaction_Model_Types;
    use Data_Readers.Status_Control;


    Output_Data_Model: constant Model_Type (Output_Tag_Type'Range) := (
        Tag_Output_Value => Exact_Length(8),
        Tag_Pubkey_Script => (
            From   => Long_Natural(Size_of_Compact_Size(1)),
            To     => Long_Natural(Size_of_Compact_Size(Scripts.Max_Data_Size) + Scripts.Max_Data_Size)
        )
    );

--    Output_Model_Max_Serialized_Data_Size: constant Positive := 8 + 1 + 83;
    Output_Model_Max_Serialized_Data_Size: constant Positive := (
        8 + Size_of_Compact_Size(Scripts.Max_Data_Size) + Scripts.Max_Data_Size
    );

    type Output_Type is
        record
            Value: Satoshi_Type;
            Pubkey_Script: Scripts.Script_Type;
        end record;

    function Empty_Output return Output_Type is (
        (
            Value => 0,
            Pubkey_Script => Scripts.Empty_Script
        )
    );


    package Data_Checkpoints is new Data_Controls.Data_Checkpoints (
        Status_Control => Data_Readers.Status_Control,
        Model => Output_Data_Model
    );

    function Output_Serialized_Data_Length(Output: Output_Type) return Long_Natural
    is (
        Long_Natural(
            8
            + Size_of_Compact_Size(Scripts.Script_Length(Output.Pubkey_Script))
                    + Scripts.Script_Length(Output.Pubkey_Script)
        )
    )
    with
        -- Ghost => True,
        Post => Output_Serialized_Data_Length'Result <= Long_Natural(Output_Model_Max_Serialized_Data_Size);

    function Output_Ghost_Tape_Match(Output: Output_Type; Offset: Natural) return Boolean
    is (
        Data_Readers.Unsigned64_Ghost_Tape_Match(Unsigned_Modular_Int64(Output.Value), Offset)
        and then
        Scripts.Script_Ghost_Tape_Match(Output.Pubkey_Script, Offset + 8)
    )
    with Ghost => True,
         Pre => Offset <= Natural'Last - Natural(Output_Serialized_Data_Length(Output)),
         Global => (Input => Ghost_Data_Tape);

    procedure Deserialize(Output: out Output_Type)
    with
        Pre => Status_OK,
        Post => (
            if Status_OK then (
                -- Output.Value is fixed length, thus checked here
                Data_Checkpoints.Final_State_Consistent(Bytes_Processed_Since(Raw_Data_Status'Old))
                and then
                Bytes_Processed_Since(Raw_Data_Status'Old) = Natural(Output_Serialized_Data_Length(Output))
                and then
                Data_Readers.Unsigned64_Ghost_Tape_Match(
                    Unsigned_Modular_Int64(Output.Value),
                    Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                        Tag_Output_Value, Data_Checkpoints.State))
                and then
                Data_Checkpoints.Element_Size_Match(
                    Tag_Pubkey_Script, Long_Natural(Scripts.Script_Serialized_Data_Length(Output.Pubkey_Script))
                )
                and then
                Scripts.Script_Ghost_Tape_Match(
                    Output.Pubkey_Script,
                    Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                        Tag_Pubkey_Script, Data_Checkpoints.State))
                and then
                Output_Ghost_Tape_Match(Output, Bytes_Processed_With(Raw_Data_Status'Old))
                and then
                (
                    for all I in Ghost_Data_Tape'Range => (
                        if (
                            I < Bytes_Processed_With(Raw_Data_Status'Old)
                            or I >= Bytes_Processed_With(Raw_Data_Status'Old)
                                        + Natural(Output_Serialized_Data_Length(Output))
                        )
                        then Ghost_Data_Tape(I) = Ghost_Data_Tape'Old(I)
                    )
                )
            )
        ),
        Depends => (
            Output => (Raw_Data_Status, Structural_Status),
            Data_Checkpoints.State =>+ (Raw_Data_Status, Structural_Status),
            Data_Tag_State =>+ (Raw_Data_Status, Structural_Status),
            Raw_Data_Status =>+ Structural_Status,
            Structural_Status =>+ Raw_Data_Status,
            Ghost_Data_Tape =>+ (Raw_Data_Status, Structural_Status)
        ),
        Global => (
            In_Out => (
                Raw_Data_Status, Structural_Status, Data_Tag_State,
                Data_Checkpoints.State, Ghost_Data_Tape
            )
        );

    -- XXX procedure Serialize ...

end Bitcoin_Like.Transaction_Outputs;
