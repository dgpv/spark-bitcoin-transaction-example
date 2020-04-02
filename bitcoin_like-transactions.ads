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

with Bitcoin_Like.Data_Accessors;
with Bitcoin_Like.Transaction_Inputs;
with Bitcoin_Like.Transaction_Outputs;
with Bitcoin_Like.Transaction_Input_Witnesses;
with Bitcoin_Like.Transaction_Model;

with Bitcoin_Like.Utils;
use Bitcoin_Like.Utils;

generic
    with package Data_Readers is new Bitcoin_Like.Data_Accessors.Readers (<>);
    with package Data_Writers is new Bitcoin_Like.Data_Accessors.Writers (<>);

    type Version_Type is range <>;
    with package Inputs is new Bitcoin_Like.Transaction_Inputs (<>);
    with package Outputs is new Bitcoin_Like.Transaction_Outputs (<>);
    with package Input_Witnesses is new Bitcoin_Like.Transaction_Input_Witnesses (<>);

    Max_Inputs  : in Positive;
    Max_Outputs : in Positive;

package Bitcoin_Like.Transactions is

    use Bitcoin_Like.Transaction_Model;
    use Transaction_Model_Types;
    use Data_Readers.Status_Control;

    -- use Inputs;
    use Outputs;
    use Input_Witnesses;

    subtype Input_Index_Type is Natural range 0..Max_Inputs-1;
    subtype Output_Index_Type is Natural range 0..Max_Outputs-1;

    subtype Input_Array_Length_Type is Natural range 0..Max_Inputs;
    subtype Output_Array_Length_Type is Natural range 0..Max_Outputs;

    type Lock_Time_Raw_Type is mod 2**32;
    subtype Lock_Time_Unlocked_Type is Lock_Time_Raw_Type range 0..0;
    subtype Lock_Time_Block_Number_Type is Lock_Time_Raw_Type range 1..500_000_000;
    subtype Lock_Time_Timestamp_Type is Lock_Time_Raw_Type range 500_000_001..(2**32-1);

    type Lock_Time_Kind is (Lock_Time_Unlocked, Lock_Time_Block_Number, Lock_Time_Timestamp);

    type Lock_Time_Variant(Kind: Lock_Time_Kind := Lock_Time_Unlocked) is
        record
            case Kind is
                when Lock_Time_Unlocked => null;
                when Lock_Time_Block_Number => Block_Number: Lock_Time_Block_Number_Type;
                when Lock_Time_Timestamp => Timestamp: Lock_Time_Timestamp_Type;
            end case;
        end record;

    Transaction_Data_Model: constant Model_Type (Transaction_Tag_Type'Range) := (
        Tag_Version => Exact_Length(4),
        Tag_Witness_Marker => (
            From   => 0,
            To     => 2
        ),
        Tag_Inputs_Count => (
            From   => 0,
            To     => Long_Natural(Size_of_Compact_Size(Max_Inputs))
        ),
        Tag_Inputs => (
            From   => 0,
            To     => Long_Natural(Inputs.Input_Model_Max_Serialized_Data_Size) * Long_Natural(Max_Inputs)
            -- To     => Model_Max_Total_Data_Size(Inputs.Input_Data_Model) * Long_Natural(Max_Inputs)
        ),
        Tag_Outputs_Count => (
            From   => 0,
            To     => Long_Natural(Size_of_Compact_Size(Max_Outputs))
        ),
        Tag_Outputs => (
            From   => 0,
            To     => Long_Natural(Outputs.Output_Model_Max_Serialized_Data_Size) * Long_Natural(Max_Outputs)
            -- To     => Model_Max_Total_Data_Size(Outputs.Output_Data_Model) * Long_Natural(Max_Outputs)
        ),
        Tag_Input_Witnesses => (
            From   => 0,
            To     => Long_Natural(Input_Witnesses.Input_Witness_Model_Max_Serialized_Data_Size) * Long_Natural(Max_Inputs)
            -- To     => Model_Max_Total_Data_Size(Input_Witnesses.Input_Witness_Data_Model) * Long_Natural(Max_Inputs)
        ),
        Tag_Lock_Time => Exact_Length(4)
    );

    type Transaction_Type is private;

    type Transaction_Inputs_Array_Type is array (Integer range <>) of Inputs.Input_Type;
    type Transaction_Outputs_Array_Type is array (Integer range <>) of Outputs.Output_Type;
    type Transaction_Input_Witnesses_Array_Type is array (Integer range <>)
        of Input_Witnesses.Input_Witness_Type;

    subtype Transaction_Inputs_Bounded_Array_Type is Transaction_Inputs_Array_Type (Input_Index_Type);
    subtype Transaction_Outputs_Bounded_Array_Type is Transaction_Outputs_Array_Type (Output_Index_Type);
    subtype Transaction_Input_Witnesses_Bounded_Array_Type
            is Transaction_Input_Witnesses_Array_Type (Input_Index_Type);

    function Transaction_Version(Transaction: Transaction_Type) return Version_Type;

    function Transaction_Inputs(Transaction: Transaction_Type) return Transaction_Inputs_Array_Type
    with Post => Transaction_Inputs'Result'Length = Transaction_Inputs_Count(Transaction);

    function Transaction_Inputs_Count(Transaction: Transaction_Type) return Input_Array_Length_Type;

    function Transaction_Input(Transaction: Transaction_Type; Index: Input_Index_Type) return Inputs.Input_Type
    with Pre => (Index < Transaction_Inputs_Count(Transaction));

    function Transaction_Outputs(Transaction: Transaction_Type) return Transaction_Outputs_Array_Type
    with Post => Transaction_Outputs'Result'Length = Transaction_Outputs_Count(Transaction);

    function Transaction_Outputs_Count(Transaction: Transaction_Type) return Output_Array_Length_Type;

    function Transaction_Output(Transaction: Transaction_Type; Index: Output_Index_Type)
        return Outputs.Output_Type
    with Pre => (Index < Transaction_Outputs_Count(Transaction));

    function Transaction_Input_Witnesses(Transaction: Transaction_Type)
        return Transaction_Input_Witnesses_Array_Type;

    function Transaction_Input_Witness(Transaction: Transaction_Type; Index: Input_Index_Type)
        return Input_Witnesses.Input_Witness_Type
    with Pre => (Index < Transaction_Inputs_Count(Transaction));

    function Transaction_Lock_Time(Transaction: Transaction_Type) return Lock_Time_Variant;

    function Empty_Transaction return Transaction_Type;

    function Is_Transaction_Input_Witnesses_Empty(
        Input_Witnesses: Transaction_Input_Witnesses_Array_Type
    ) return Boolean
    with Post => (
        if Is_Transaction_Input_Witnesses_Empty'Result then (
            for all Input_Witness of Input_Witnesses => Is_Input_Witness_Empty(Input_Witness)
        )
        else (
            for some Input_Witness of Input_Witnesses => not Is_Input_Witness_Empty(Input_Witness)
        )
    );

    function Transaction_Has_Witness(Transaction: Transaction_Type) return Boolean
    is (not Is_Transaction_Input_Witnesses_Empty(Transaction_Input_Witnesses(Transaction)))
    with Post => (
        if Transaction_Has_Witness'Result then (
            for some Input_Witness of Transaction_Input_Witnesses(Transaction)
                => not Is_Input_Witness_Empty(Input_Witness)
        )
        else (
            for all Input_Witness of Transaction_Input_Witnesses(Transaction)
                => Is_Input_Witness_Empty(Input_Witness)
        )
    );

    package Data_Checkpoints is new Data_Controls.Data_Checkpoints (
        Status_Control => Data_Readers.Status_Control,
        Model => Transaction_Data_Model
    );

    type Inputs_Serialized_Cumulative_Sums_Type is array (Natural range <>) of Long_Natural;

    function Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array: Transaction_Inputs_Array_Type)
        return Inputs_Serialized_Cumulative_Sums_Type
    with 
        Ghost => True,
        Pre => (
            Inputs_Array'Last < Max_Inputs
            and Inputs_Array'First = 0
        ),
        Post => (
            Inputs_Serialized_Cumulative_Data_Lengths'Result'First = Inputs_Array'First
            and then
            Inputs_Serialized_Cumulative_Data_Lengths'Result'Last = Inputs_Array'Last
            and then
            (
                for all I in 0..Inputs_Array'Last =>
                    Inputs_Serialized_Cumulative_Data_Lengths'Result(I)
                        <= Long_Natural(Inputs.Input_Model_Max_Serialized_Data_Size)
                                * Long_Natural(I + 1)
            )
            and then
            (
                for all I in 0..Inputs_Array'Last =>
                    Inputs_Serialized_Cumulative_Data_Lengths'Result(I)
                        = Inputs.Input_Serialized_Data_Length(Inputs_Array(I))
                           + (
                                if I = 0
                                then 0
                                else Inputs_Serialized_Cumulative_Data_Lengths'Result(I - 1)
                           )
            )
        );

    procedure Inputs_Serialized_Cumulative_Data_Lengths_Eq_Lemma(
        Inputs_Array_First  : Transaction_Inputs_Array_Type;
        Inputs_Array_Second : Transaction_Inputs_Array_Type
    )
    with
        Ghost => True,
        Pre => (
            (
                if Inputs_Array_First'Length = Inputs_Array_Second'Length
                then Inputs_Array_First'Length >= 1
                else Inputs_Array_First'Length > 1
            )
            and then Inputs_Array_First'Last < Max_Inputs
            and then Inputs_Array_First'First = 0
            and then Inputs_Array_Second'First = Inputs_Array_First'First 
            and then (
                Inputs_Array_Second'Last = Inputs_Array_First'Last - 1
                or Inputs_Array_Second'Last = Inputs_Array_First'Last
            )
            and then (
                (
                    for all I in 0..Inputs_Array_Second'Last =>
                        Inputs."="(Inputs_Array_First(I), Inputs_Array_Second(I))
                )
                and then
                (
                    for all I in 0..Inputs_Array_First'Last =>
                        Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_First)(I)
                            = Inputs.Input_Serialized_Data_Length(Inputs_Array_First(I))
                               + (
                                    if I = 0
                                    then 0
                                    else Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_First)(I - 1)
                               )
                )
                and then
                (
                    for all I in 0..Inputs_Array_Second'Last =>
                        Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_Second)(I)
                            = Inputs.Input_Serialized_Data_Length(Inputs_Array_Second(I))
                               + (
                                    if I = 0
                                    then 0
                                    else Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_Second)(I - 1)
                               )
                )
            )
        ),
        Post => (
            (
                for all I in 0..Inputs_Array_Second'Last =>
                    Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_First)(I)
                        = Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_Second)(I)
            )
            and then
            (
                if Inputs_Array_First'Length = Inputs_Array_Second'Length
                then
                Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_First)(Inputs_Array_First'Last)
                    = Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_Second)(Inputs_Array_Second'Last)
                else
                Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_First)(Inputs_Array_First'Last)
                    = Inputs.Input_Serialized_Data_Length(Inputs_Array_First(Inputs_Array_First'Last))
                        + Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_Second)(Inputs_Array_Second'Last)
            )
        );

    function Inputs_Serialized_Data_Length(Inputs_Array: Transaction_Inputs_Array_Type) return Long_Natural
    is (
        if Inputs_Array'Length = 0 then 0
        else Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array)(Inputs_Array'Last)
    )
    with
    Ghost => True,
    Pre => Inputs_Array'First = 0 and Inputs_Array'Last < Max_Inputs,
    Post => (
        Inputs_Serialized_Data_Length'Result
            <= Long_Natural(Inputs.Input_Model_Max_Serialized_Data_Size) * Inputs_Array'Length
    );

    type Outputs_Serialized_Cumulative_Sums_Type is array (Natural range <>) of Long_Natural;

    function Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array: Transaction_Outputs_Array_Type)
        return Outputs_Serialized_Cumulative_Sums_Type
    with
        Pre => (
            Outputs_Array'Last < Max_Outputs
            and Outputs_Array'First = 0
        ),
        Post => (
            Outputs_Serialized_Cumulative_Data_Lengths'Result'First = Outputs_Array'First
            and then
            Outputs_Serialized_Cumulative_Data_Lengths'Result'Last = Outputs_Array'Last
            and then
            (
                for all I in 0..Outputs_Array'Last =>
                    Outputs_Serialized_Cumulative_Data_Lengths'Result(I)
                        <= Long_Natural(Outputs.Output_Model_Max_Serialized_Data_Size)
                                * Long_Natural(I + 1)
            )
            and then
            (
                for all I in 0..Outputs_Array'Last =>
                    Outputs_Serialized_Cumulative_Data_Lengths'Result(I)
                        = Outputs.Output_Serialized_Data_Length(Outputs_Array(I))
                           + (
                                if I = 0
                                then 0
                                else Outputs_Serialized_Cumulative_Data_Lengths'Result(I - 1)
                           )
            )
        );

    procedure Outputs_Serialized_Cumulative_Data_Lengths_Eq_Lemma(
        Outputs_Array_First  : Transaction_Outputs_Array_Type;
        Outputs_Array_Second : Transaction_Outputs_Array_Type
    )
    with
        Ghost => True,
        Pre => (
            Outputs_Array_First'Length > 1
            and then Outputs_Array_First'Last < Max_Outputs
            and then Outputs_Array_First'First = 0
            and then Outputs_Array_Second'First = Outputs_Array_First'First
            and then Outputs_Array_Second'Last = Outputs_Array_First'Last - 1
            and then (
                (
                    for all I in 0..Outputs_Array_Second'Last =>
                        Outputs."="(Outputs_Array_First(I), Outputs_Array_Second(I))
                )
                and then
                (
                    for all I in 0..Outputs_Array_First'Last =>
                        Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array_First)(I)
                            = Outputs.Output_Serialized_Data_Length(Outputs_Array_First(I))
                               + (
                                    if I = 0
                                    then 0
                                    else Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array_First)(I - 1)
                               )
                )
                and then
                (
                    for all I in 0..Outputs_Array_Second'Last =>
                        Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array_Second)(I)
                            = Outputs.Output_Serialized_Data_Length(Outputs_Array_Second(I))
                               + (
                                    if I = 0
                                    then 0
                                    else Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array_Second)(I - 1)
                               )
                )
            )
        ),
        Post => (
            (
                for all I in 0..Outputs_Array_Second'Last =>
                    Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array_First)(I)
                        = Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array_Second)(I)
            )
            and then
            Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array_First)(Outputs_Array_First'Last)
                = Outputs.Output_Serialized_Data_Length(Outputs_Array_First(Outputs_Array_First'Last))
                    + Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array_Second)(Outputs_Array_Second'Last)
        );

    function Outputs_Serialized_Data_Length(Outputs_Array: Transaction_Outputs_Array_Type) return Long_Natural
    is (
        if Outputs_Array'Length = 0 then 0
        else Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array)(Outputs_Array'Last)
    )
    with
    Pre => Outputs_Array'First = 0 and Outputs_Array'Last < Max_Outputs,
    Post => (
        Outputs_Serialized_Data_Length'Result
            <= Long_Natural(Outputs.Output_Model_Max_Serialized_Data_Size) * Outputs_Array'Length
    );

    type Input_Witnesses_Serialized_Cumulative_Sums_Type is array (Natural range <>) of Long_Natural;

    function Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array: Transaction_Input_Witnesses_Array_Type)
        return Input_Witnesses_Serialized_Cumulative_Sums_Type
    with 
        Ghost => True,
        Pre => (
            Input_Witnesses_Array'Last < Max_Inputs
            and Input_Witnesses_Array'First = 0
        ),
        Post => (
            Input_Witnesses_Serialized_Cumulative_Data_Lengths'Result'First = Input_Witnesses_Array'First
            and then
            Input_Witnesses_Serialized_Cumulative_Data_Lengths'Result'Last = Input_Witnesses_Array'Last
            and then
            (
                for all I in 0..Input_Witnesses_Array'Last =>
                    Input_Witnesses_Serialized_Cumulative_Data_Lengths'Result(I)
                        <= Long_Natural(Input_Witnesses.Input_Witness_Model_Max_Serialized_Data_Size)
                                * Long_Natural(I + 1)
            )
            and then
            (
                for all I in 0..Input_Witnesses_Array'Last =>
                    Input_Witnesses_Serialized_Cumulative_Data_Lengths'Result(I)
                        = Input_Witnesses.Input_Witness_Serialized_Data_Length(Input_Witnesses_Array(I))
                           + (
                                if I = 0
                                then 0
                                else Input_Witnesses_Serialized_Cumulative_Data_Lengths'Result(I - 1)
                           )
            )
        );

    procedure Input_Witnesses_Serialized_Cumulative_Data_Lengths_Eq_Lemma(
        Input_Witnesses_Array_First  : Transaction_Input_Witnesses_Array_Type;
        Input_Witnesses_Array_Second : Transaction_Input_Witnesses_Array_Type
    )
    with
        Ghost => True,
        Pre => (
            Input_Witnesses_Array_First'Length > 1
            and then Input_Witnesses_Array_First'Last < Max_Inputs
            and then Input_Witnesses_Array_First'First = 0
            and then Input_Witnesses_Array_Second'First = Input_Witnesses_Array_First'First
            and then Input_Witnesses_Array_Second'Last = Input_Witnesses_Array_First'Last - 1
            and then (
                (
                    for all I in 0..Input_Witnesses_Array_Second'Last =>
                        Input_Witnesses."="(Input_Witnesses_Array_First(I), Input_Witnesses_Array_Second(I))
                )
                and then
                (
                    for all I in 0..Input_Witnesses_Array_First'Last =>
                        Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array_First)(I)
                            = Input_Witnesses.Input_Witness_Serialized_Data_Length(Input_Witnesses_Array_First(I))
                               + (
                                    if I = 0
                                    then 0
                                    else Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array_First)(I - 1)
                               )
                )
                and then
                (
                    for all I in 0..Input_Witnesses_Array_Second'Last =>
                        Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array_Second)(I)
                            = Input_Witnesses.Input_Witness_Serialized_Data_Length(Input_Witnesses_Array_Second(I))
                               + (
                                    if I = 0
                                    then 0
                                    else Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array_Second)(I - 1)
                               )
                )
            )
        ),
        Post => (
            (
                for all I in 0..Input_Witnesses_Array_Second'Last =>
                    Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array_First)(I)
                        = Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array_Second)(I)
            )
            and then
            Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array_First)(Input_Witnesses_Array_First'Last)
                = Input_Witnesses.Input_Witness_Serialized_Data_Length(Input_Witnesses_Array_First(Input_Witnesses_Array_First'Last))
                    + Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array_Second)(Input_Witnesses_Array_Second'Last)
        );

    function Input_Witnesses_Serialized_Data_Length(Input_Witnesses_Array: Transaction_Input_Witnesses_Array_Type) return Long_Natural
    is (
        if Input_Witnesses_Array'Length = 0 then 0
        else Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array)(Input_Witnesses_Array'Last)
    )
    with
    Ghost => True,
    Pre => (
            Input_Witnesses_Array'Last < Max_Inputs
            and Input_Witnesses_Array'First = 0
    ),
    Post => (
        Input_Witnesses_Serialized_Data_Length'Result
            <= Long_Natural(Input_Witnesses.Input_Witness_Model_Max_Serialized_Data_Size) * Input_Witnesses_Array'Length
    );

    procedure Deserialize_Inputs(Transaction: in out Transaction_Type; Inputs_Count: in Input_Array_Length_Type)
    with
        Pre => (Status_OK and then Inputs_Count <= Max_Inputs),
        Post => (
            if Status_OK then (
                Transaction_Inputs_Count(Transaction) = Inputs_Count
                and then
                Current_Data_Tag = Data_Checkpointing.Current_Data_Tag(Data_Tag_State'Old)
                and then
                Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction))
                    = Long_Natural(Bytes_Processed_Since(Raw_Data_Status'Old))
                and then
                Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction))
                    <= Long_Natural(Inputs.Input_Model_Max_Serialized_Data_Size) * Long_Natural(Inputs_Count)
            )
        ),
        Depends => (
            Transaction =>+ (Inputs_Count, Raw_Data_Status, Structural_Status),
            Inputs.Data_Checkpoints.State =>+ (Raw_Data_Status, Structural_Status, Inputs_Count),
            Data_Tag_State =>+ (Inputs_Count, Raw_Data_Status, Structural_Status),
            Raw_Data_Status =>+ (Structural_Status, Inputs_Count),
            Structural_Status =>+ (Raw_Data_Status, Inputs_Count),
            Ghost_Data_Tape =>+ (Raw_Data_Status, Structural_Status, Inputs_Count)
        ),
        Global => (
            In_Out => (
                Raw_Data_Status, Structural_Status, Inputs.Data_Checkpoints.State, Data_Tag_State,
                Ghost_Data_Tape
            )
        );

    procedure Deserialize_Outputs(Transaction: in out Transaction_Type; Outputs_Count: in Output_Array_Length_Type)
    with
        Pre => (Status_OK and then Outputs_Count <= Max_Outputs),
        Post => (
            if Status_OK then (
                Transaction_Outputs_Count(Transaction) = Outputs_Count
                and then
                Current_Data_Tag = Data_Checkpointing.Current_Data_Tag(Data_Tag_State'Old)
                and then
                Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction))
                    <= Long_Natural(Outputs.Output_Model_Max_Serialized_Data_Size) * Long_Natural(Outputs_Count)
                and then
                Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction))
                    = Long_Natural(Bytes_Processed_Since(Raw_Data_Status'Old))
                and then
                -- Unfortunately we cannot use Transaction'Old'Update here,
                -- because Transaction is a private record
                Transaction_Version(Transaction) = Transaction_Version(Transaction'Old)
                and then
                Transaction_Inputs_Count(Transaction) = Transaction_Inputs_Count(Transaction'Old)
                and then
                Transaction_Inputs(Transaction) = Transaction_Inputs(Transaction'Old)
            )
        ),
        Depends => (
            Transaction =>+ (Outputs_Count, Raw_Data_Status, Structural_Status),
            Outputs.Data_Checkpoints.State =>+ (Raw_Data_Status, Structural_Status, Outputs_Count),
            Data_Tag_State =>+ (Outputs_Count, Raw_Data_Status, Structural_Status),
            Raw_Data_Status =>+ (Structural_Status, Outputs_Count),
            Structural_Status =>+ (Raw_Data_Status, Outputs_Count),
            Ghost_Data_Tape =>+ (Raw_Data_Status, Structural_Status, Outputs_Count)
        ),
        Global => (
            In_Out => (
                Raw_Data_Status, Structural_Status, Outputs.Data_Checkpoints.State, Data_Tag_State,
                Ghost_Data_Tape
            )
        );

    procedure Deserialize_Input_Witnesses(Transaction: in out Transaction_Type)
    with
        Pre => (Status_OK and Transaction_Inputs_Count(Transaction) > 0),
        Post => (
            if Status_OK then (
                Transaction_Inputs_Count(Transaction) = Transaction_Inputs_Count(Transaction'Old)
                and then
                Transaction_Outputs_Count(Transaction) = Transaction_Outputs_Count(Transaction'Old)
                and then
                Transaction_Input_Witnesses(Transaction)'Length = Transaction_Inputs_Count(Transaction)
                and then
                Current_Data_Tag = Data_Checkpointing.Current_Data_Tag(Data_Tag_State'Old)
                and then
                Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction))
                    <= Long_Natural(Input_Witnesses.Input_Witness_Model_Max_Serialized_Data_Size)
                        * Long_Natural(Transaction_Inputs_Count(Transaction))
                and then
                Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction))
                    = Long_Natural(Bytes_Processed_Since(Raw_Data_Status'Old))
                and then
                Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction))
                     = Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction'Old))
                and then
                Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction))
                     = Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction'Old))
            )
        ),
        Depends => (
            Transaction =>+ (Raw_Data_Status, Structural_Status),
            Input_Witnesses.Data_Checkpoints.State =>+ (Raw_Data_Status, Structural_Status, Transaction),
            Data_Tag_State =>+ (Transaction, Raw_Data_Status, Structural_Status),
            Raw_Data_Status =>+ (Structural_Status, Transaction),
            Structural_Status =>+ (Raw_Data_Status, Transaction),
            Ghost_Data_Tape =>+ (Raw_Data_Status, Structural_Status, Transaction)
        ),
        Global => (
            In_Out => (
                Raw_Data_Status, Structural_Status, Input_Witnesses.Data_Checkpoints.State, Data_Tag_State,
                Ghost_Data_Tape
            )
        );

    procedure Deserialize(Transaction: out Transaction_Type)
    with
        Pre => Status_OK and Bytes_Processed = 0,
        Post => (
            if Status_OK then (
                Data_Checkpoints.Final_State_Consistent(
                    Bytes_Processed_Since(Raw_Data_Status'Old)
                )
                and then
                Data_Checkpoints.Element_Size_Match(
                    Tag_Witness_Marker,
                    (if Transaction_Has_Witness(Transaction) then 2 else 0)
                )
                and then
                Data_Checkpoints.Element_Size_Match(
                    Tag_Inputs_Count,
                    Long_Natural(Size_of_Compact_Size(Transaction_Inputs_Count(Transaction)))
                )
                and then
                Data_Checkpoints.Element_Size_Match(
                    Tag_Inputs,
                    Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction))
                )
                and then
                Data_Checkpoints.Element_Size_Match(
                    Tag_Outputs_Count,
                    Long_Natural(Size_of_Compact_Size(Transaction_Outputs_Count(Transaction)))
                )
                and then
                Data_Checkpoints.Element_Size_Match(
                    Tag_Outputs,
                    Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction))
                )
                and then
                Data_Checkpoints.Element_Size_Match(
                    Tag_Input_Witnesses,
                    Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction))
                )
                and then
                (
                    if not Transaction_Has_Witness(Transaction)
                    then Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction)) = 0
                )
            )
        ),
        Depends => (
            Transaction => (Raw_Data_Status, Structural_Status),
            Data_Checkpoints.State =>+ (Raw_Data_Status, Structural_Status),
            Inputs.Data_Checkpoints.State =>+ (Raw_Data_Status, Structural_Status),
            Outputs.Data_Checkpoints.State =>+ (Raw_Data_Status, Structural_Status),
            Input_Witnesses.Data_Checkpoints.State =>+ (Raw_Data_Status, Structural_Status),
            Data_Tag_State =>+ (Raw_Data_Status, Structural_Status),
            Raw_Data_Status =>+ Structural_Status,
            Structural_Status =>+ Raw_Data_Status,
            Ghost_Data_Tape =>+ (Raw_Data_Status, Structural_Status)
        ),
        Global => (
            In_Out => (
                Raw_Data_Status, Structural_Status, Data_Tag_State, Ghost_Data_Tape,
                Data_Checkpoints.State,
                Inputs.Data_Checkpoints.State,
                Outputs.Data_Checkpoints.State,
                Input_Witnesses.Data_Checkpoints.State
            )
        );

    -- XXX procedure Serialize ...

private
    type Transaction_Type is
        record
            Version: Version_Type;
            Inputs_Count: Input_Array_Length_Type;
            Inputs: Transaction_Inputs_Bounded_Array_Type;
            Outputs_Count: Output_Array_Length_Type;
            Outputs: Transaction_Outputs_Bounded_Array_Type;
            Input_Witnesses_Count: Input_Array_Length_Type;
            Input_Witnesses: Transaction_Input_Witnesses_Bounded_Array_Type;
            Lock_Time: Lock_Time_Variant;
        end record;

    function Empty_Transaction return Transaction_Type
    is (
        (
            Version => Version_Type'First,
            Inputs_Count => 0, Inputs => (others => Inputs.Empty_Input),
            Outputs_Count => 0, Outputs => (others => Empty_Output),
            Input_Witnesses_Count => 0, Input_Witnesses => (others => Empty_Input_Witness),
            Lock_Time => (Kind => Lock_Time_Unlocked)
        )
    );

    function Transaction_Version(Transaction: Transaction_Type) return Version_Type
    is (Transaction.Version);

    function Transaction_Inputs_Count(Transaction: Transaction_Type) return Input_Array_Length_Type
    is (Transaction.Inputs_Count);

    function Transaction_Input(Transaction: Transaction_Type; Index: Input_Index_Type) return Inputs.Input_Type
    is (Transaction.Inputs(Index));

    function Transaction_Inputs(Transaction: Transaction_Type) return Transaction_Inputs_Array_Type
    is (
        if Transaction.Inputs_Count = 0
        then Transaction.Inputs(0..-1)
        else Transaction.Inputs(0..Transaction.Inputs_Count - 1)
    );

    function Transaction_Outputs_Count(Transaction: Transaction_Type) return Output_Array_Length_Type
    is (Transaction.Outputs_Count);

    function Transaction_Output(Transaction: Transaction_Type; Index: Output_Index_Type)
        return Outputs.Output_Type
    is (Transaction.Outputs(Index));

    function Transaction_Outputs(Transaction: Transaction_Type) return Transaction_Outputs_Array_Type
    is (
        if Transaction.Outputs_Count = 0
        then Transaction.Outputs(0..-1)
        else Transaction.Outputs(0..Transaction.Outputs_Count - 1)
    );

    function Transaction_Input_Witness(Transaction: Transaction_Type; Index: Input_Index_Type)
        return Input_Witnesses.Input_Witness_Type
    is (Transaction.Input_Witnesses(Index));

    function Transaction_Input_Witnesses(Transaction: Transaction_Type)
        return Transaction_Input_Witnesses_Array_Type
    is (
        if Transaction.Input_Witnesses_Count = 0
        then Transaction.Input_Witnesses(0..-1)
        else Transaction.Input_Witnesses(0..Transaction.Input_Witnesses_Count - 1)
    );

    function Transaction_Lock_Time(Transaction: Transaction_Type) return Lock_Time_Variant
    is (
        Transaction.Lock_Time
    );

end Bitcoin_Like.Transactions;
