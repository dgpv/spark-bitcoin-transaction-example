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

with Utils; use Utils;

with Data_Controls;

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

    type Previous_Output_Index_Type is range <>;

package Bitcoin_Like.Transaction_Inputs is

    use Bitcoin_Like.Transaction_Model;
    use Transaction_Model_Types;
    use Data_Readers.Status_Control;

    subtype Transaction_Txid_Octet is Byte_Type;
    type Transaction_Txid_Type is array (1..32) of Transaction_Txid_Octet;

    package Txid_Reader is new Data_Readers.Block_Reader(Octet_Type => Transaction_Txid_Octet);

    type Input_Sequence_Type is range 0..2**32-1;
    subtype Input_Sequence_Consensus_Constrained_Type is Input_Sequence_Type range 0..2**31-1;
    subtype Input_Sequence_Consensus_Unconstrained_Type is Input_Sequence_Type range 2**31..2**32-1;

    type Previous_Output_Point_Type is
        record
            Txid: Transaction_Txid_Type;
            Index: Previous_Output_Index_Type;
        end record;

    type Input_Type is
        record
            Previous_Output_Point: Previous_Output_Point_Type;
            Signature_Script: Scripts.Script_Type;
            Sequence: Input_Sequence_Type;
        end record;

    function Empty_Input return Input_Type is (
        (
            Previous_Output_Point => (Txid => (others => 0), Index => 0),
            Signature_Script => Scripts.Empty_Script,
            Sequence => 0
        )
    );

    Input_Data_Model: constant Model_Type (Input_Tag_Type'Range) := (
        Tag_Txid             => Exact_Length(Transaction_Txid_Type'Length),
        Tag_Prevout_Index    => Exact_Length(4),
        Tag_Signature_Script => (
            From   => Long_Natural(Size_of_Compact_Size(1)),
            To     => Long_Natural(Size_of_Compact_Size(Scripts.Max_Data_Size) + Scripts.Max_Data_Size)
        ),
        Tag_Sequence_Number  => Exact_Length(4)
    );

--    Input_Model_Max_Serialized_Data_Size: constant Positive := 32 + 4 + 3 + 1650 + 4;
    Input_Model_Max_Serialized_Data_Size: constant Positive := (
        Transaction_Txid_Type'Length
            + 4
            + Size_of_Compact_Size(Scripts.Max_Data_Size) + Scripts.Max_Data_Size
            + 4
    );

    -- If we want to ensure that the fields are only set within appropriate contexts,
    -- we can use setter procedures that would check Current_Data_Tag in precondition.
    --
    -- It means that we will have to copy data for bigger data fields, though (now we can just
    -- pass the Signature_Script field directly to appropriate deserialize procedure, for example).
    --
    -- the specification for one possible setter procedure:
    --
    -- procedure Set_Previous_Output_Index(Input: in out Input_Type; Index: in Previous_Output_Index_Type)
    -- with
    --      Pre => Data_Model."="(Current_Data_Tag, Tag_Prevout_Index),
    --      Post => Input = (Input'Old with delta
    --                            Previous_Output_Point => (
    --                                 Input'Old.Previous_Output_Point with delta Index => Index));

    package Data_Checkpoints is new Data_Controls.Data_Checkpoints (
        Status_Control => Data_Readers.Status_Control,
        Model => Input_Data_Model
    );

    function Input_Serialized_Data_Length(Input: Input_Type) return Long_Natural
    is (
        Long_Natural(
            Transaction_Txid_Type'Length
            + 4
            + Size_of_Compact_Size(Scripts.Script_Length(Input.Signature_Script))
                + Scripts.Script_Length(Input.Signature_Script)
            + 4
        )
    )
    with
        -- Ghost => True,
        Post => (
            Input_Serialized_Data_Length'Result <= Long_Natural(Input_Model_Max_Serialized_Data_Size)
        );

    function Input_Ghost_Tape_Match(Input: Input_Type; Offset: Natural) return Boolean
    is (
        Txid_Reader.Block_Ghost_Tape_Match(
            Txid_Reader.Block_Type(Input.Previous_Output_Point.Txid),
            Offset)
        and then
        Data_Readers.Unsigned32_Ghost_Tape_Match(
            Unsigned_Ranged_Int32(Input.Previous_Output_Point.Index),
            Offset + Input.Previous_Output_Point.Txid'Length)
        and then
        Scripts.Script_Ghost_Tape_Match(
            Input.Signature_Script,
            Offset + 4 + Input.Previous_Output_Point.Txid'Length)
        and then
        Data_Readers.Unsigned32_Ghost_Tape_Match(
            Unsigned_Ranged_Int32(Input.Sequence),
            Offset + 4 + Input.Previous_Output_Point.Txid'Length
            + Scripts.Script_Serialized_Data_Length(Input.Signature_Script))
    )
    with Ghost => True,
         Pre => Offset <= Natural'Last - Natural(Input_Serialized_Data_Length(Input)),
         Global => (Input => Ghost_Data_Tape);


    procedure Deserialize(Input: out Input_Type)
    with
        Pre => Status_OK,
        Post => (
            if Status_OK then (
                -- txid, prevout_index and sequence_number lengths are fixed
                -- and checked by Final_State_Consistent
                Data_Checkpoints.Final_State_Consistent(
                    Bytes_Processed_Since(Raw_Data_Status'Old)
                )
                and then
                Txid_Reader.Block_Ghost_Tape_Match(
                    Txid_Reader.Block_Type(Input.Previous_Output_Point.Txid),
                    Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                        Tag_Txid, Data_Checkpoints.State))
                and then
                Data_Readers.Unsigned32_Ghost_Tape_Match(
                    Unsigned_Ranged_Int32(Input.Previous_Output_Point.Index),
                    Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                        Tag_Prevout_Index, Data_Checkpoints.State))
                and then
                Data_Checkpoints.Element_Size_Match(
                    Tag_Signature_Script,
                    Long_Natural(
                        Size_of_Compact_Size(Scripts.Script_Length(Input.Signature_Script))
                            + Scripts.Script_Length(Input.Signature_Script)
                    )
                )
                and then
                Scripts.Script_Ghost_Tape_Match(
                    Input.Signature_Script,
                    Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                        Tag_Signature_Script, Data_Checkpoints.State))
                and then
                Data_Readers.Unsigned32_Ghost_Tape_Match(
                    Unsigned_Ranged_Int32(Input.Sequence),
                    Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                        Tag_Sequence_Number, Data_Checkpoints.State))
                and then
                Input_Serialized_Data_Length(Input)
                    = Long_Natural(Bytes_Processed_Since(Raw_Data_Status'Old))
                and then
                Input_Ghost_Tape_Match(Input, Bytes_Processed_With(Raw_Data_Status'Old))
            )
        ),
        Depends => (
            Input => (Raw_Data_Status, Structural_Status),
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

end Bitcoin_Like.Transaction_Inputs;
