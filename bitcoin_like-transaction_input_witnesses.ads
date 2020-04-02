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

    Max_Witness_Data_Size   : in Positive;
    Max_Witness_Stack_Items : in Positive;
package Bitcoin_Like.Transaction_Input_Witnesses is

    use Bitcoin_Like.Transaction_Model;
    use Transaction_Model_Types;
    use Data_Readers.Status_Control;

    subtype Input_Witness_Stack_Index_Type is Positive range 1..Max_Witness_Stack_Items;
    subtype Input_Witness_Stack_Length_Type is Natural range 0..Max_Witness_Stack_Items;
    subtype Input_Witness_Data_Blob_Length_Type is Natural range 0..Max_Witness_Data_Size;

    subtype Input_Witness_Data_Octet is Byte_Type;
    type Input_Witness_Stack_Item_Data_Type is array (Natural range <>) of Input_Witness_Data_Octet;

    type Input_Witness_Stack_Item_Type is private;
    type Input_Witness_Stack_Items_Array_Type is array (Natural range <>) of Input_Witness_Stack_Item_Type;

    type Input_Witness_Stack_Type is private;

    type Input_Witness_Type is
        record
            Stack        : Input_Witness_Stack_Type;
            Script       : Scripts.Script_Type;
        end record;

    function Empty_Input_Witness_Stack return Input_Witness_Stack_Type;

    function Empty_Input_Witness return Input_Witness_Type is (
        (
            Stack => Empty_Input_Witness_Stack,
            Script => Scripts.Empty_Script
        )
    )
    with Post => Is_Input_Witness_Empty(Empty_Input_Witness'Result);

    Input_Witness_Data_Model: constant Model_Type (Input_Witness_Tag_Type'Range) := (
        Tag_Input_Witness_Stack_Length => (
            From   => Long_Natural(Size_of_Compact_Size(0)),
            To     => Long_Natural(Size_of_Compact_Size(Max_Witness_Stack_Items + 1))
        ),
        Tag_Input_Witness_Stack => (
            From   => 0,
            To     => Long_Natural(
                Max_Witness_Data_Size * Max_Witness_Stack_Items
                + Size_of_Compact_Size(Max_Witness_Data_Size) * Max_Witness_Stack_Items
            )
        ),
        Tag_Input_Witness_Script => (
            From   => 0,
            To     => Long_Natural(Size_of_Compact_Size(Scripts.Max_Data_Size) + Scripts.Max_Data_Size)
        )
    );

--    Input_Witness_Model_Max_Serialized_Data_Size: constant Positive := 1 + 80*100 + 1*100 + 3 + 3600;
    Input_Witness_Model_Max_Serialized_Data_Size: constant Positive := (
        Size_of_Compact_Size(Max_Witness_Stack_Items + 1)
        + (
            Max_Witness_Data_Size * Max_Witness_Stack_Items
            + Size_of_Compact_Size(Max_Witness_Data_Size) * Max_Witness_Stack_Items
        )
        + Size_of_Compact_Size(Scripts.Max_Data_Size) + Scripts.Max_Data_Size
    );

    package Data_Checkpoints is new Data_Controls.Data_Checkpoints (
        Status_Control => Data_Readers.Status_Control,
        Model => Input_Witness_Data_Model
    );

    -- Using Util.Array_Summation is too problematic when some types are in private part.
    -- Even if we use Array_Summation in private part and declare funcs + postconditions
    -- in public part, this becomes too messy and too heavy for the prover,
    -- therefore we tolerate some code duplication to make proving easier.

    type Serialized_Length_Cumulative_Sums_Type is array (Natural range <>) of Long_Natural;

    function Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(
        Stack_Items: Input_Witness_Stack_Items_Array_Type
    )
    return Serialized_Length_Cumulative_Sums_Type
    with
        Pre => (
            Stack_Items'First = 1
            and then
            Stack_Items'Last <= Max_Witness_Stack_Items
        ),
        Post => (
            Input_Witness_Stack_Serialized_Data_Cumulative_Lengths'Result'First = Stack_Items'First
            and then
            Input_Witness_Stack_Serialized_Data_Cumulative_Lengths'Result'Last = Stack_Items'Last
            and then
            (
                for all I in Stack_Items'Range =>
                     Input_Witness_Stack_Serialized_Data_Cumulative_Lengths'Result(I)
                        <= Long_Natural(I * (Size_of_Compact_Size(Max_Witness_Data_Size) + Max_Witness_Data_Size))
            )
            and then
            (
                for all I in Stack_Items'Range =>
                     Input_Witness_Stack_Serialized_Data_Cumulative_Lengths'Result(I)
                     = Long_Natural(Size_of_Compact_Size(Input_Witness_Stack_Item_Size(Stack_Items(I))))
                       + Long_Natural(Input_Witness_Stack_Item_Size(Stack_Items(I)))
                       + (
                           if I = 1
                           then 0
                           else Input_Witness_Stack_Serialized_Data_Cumulative_Lengths'Result(I-1)
                       )
            )
        );

    function Input_Witness_Stack_Serialized_Data_Length(
        Stack_Items: Input_Witness_Stack_Items_Array_Type
    ) return Long_Natural
    with
        Ghost => True,
        Pre => (
            Stack_Items'First = Input_Witness_Stack_Index_Type'First
            and then
            Stack_Items'Last <= Input_Witness_Stack_Index_Type'Last
        ),
        Post => (
                Input_Witness_Stack_Serialized_Data_Length'Result <= Long_Natural(Natural'Last)
                and then
                Input_Witness_Stack_Serialized_Data_Length'Result
                    = (
                        if Stack_Items'Length = 0
                        then 0
                        else Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(
                            Stack_Items
                        ) (Stack_Items'Last)
                    )
        );

    procedure Cumulative_Lengths_Eq_Lemma (
        Cumulative_Sums_First: Serialized_Length_Cumulative_Sums_Type;
        Cumulative_Sums_Second: Serialized_Length_Cumulative_Sums_Type;
        Stack_Items: Input_Witness_Stack_Items_Array_Type
    )
    with
        Ghost => True,
        Pre => (
                Stack_Items'Last <= Max_Witness_Stack_Items
                and then Stack_Items'First = 1
                and then Stack_Items'First = Cumulative_Sums_First'First
                and then Stack_Items'Last = Cumulative_Sums_First'Last
                and then Stack_Items'First = Cumulative_Sums_Second'First
                and then Stack_Items'Last = Cumulative_Sums_Second'Last
                and then Stack_Items'Length = Cumulative_Sums_Second'Length
                and then Stack_Items'Length = Cumulative_Sums_Second'Length
                and then (
                    for all I in 1..Stack_Items'Last =>
                        Cumulative_Sums_First(I) <=
                            Long_Natural(
                                I * (Size_of_Compact_Size(Max_Witness_Data_Size) + Max_Witness_Data_Size))
                )
                and then (
                    for all I in 1..Stack_Items'Last =>
                        Cumulative_Sums_Second(I) <=
                            Long_Natural(
                                I * (Size_of_Compact_Size(Max_Witness_Data_Size) + Max_Witness_Data_Size))
                )
                and then (
                    for all I in 1..Stack_Items'Last =>
                         Cumulative_Sums_First(I)
                             = Long_Natural(Size_of_Compact_Size(Input_Witness_Stack_Item_Size(Stack_Items(I))))
                               + Long_Natural(Input_Witness_Stack_Item_Size(Stack_Items(I)))
                               + (if I = 1 then 0 else Cumulative_Sums_First(I-1))
                )
                and then (
                    for all I in 1..Stack_Items'Last =>
                         Cumulative_Sums_Second(I)
                             = Long_Natural(Size_of_Compact_Size(Input_Witness_Stack_Item_Size(Stack_Items(I))))
                               + Long_Natural(Input_Witness_Stack_Item_Size(Stack_Items(I)))
                               + (if I = 1 then 0 else Cumulative_Sums_Second(I-1))
                )
        ),
        Post => (Cumulative_Sums_First = Cumulative_Sums_Second);

    procedure Input_Witness_Stack_Serialized_Cumulative_Data_Lengths_Eq_Lemma(
        Stack_Items_First  : Input_Witness_Stack_Items_Array_Type;
        Stack_Items_Second : Input_Witness_Stack_Items_Array_Type
    )
    with
        Ghost => True,
        Pre => (
            Stack_Items_First'Last <= Max_Witness_Stack_Items
            and then Stack_Items_First'First = 1
            and then Stack_Items_Second'First = Stack_Items_First'First
            and then Stack_Items_Second'Last = Stack_Items_First'Last
            and then Stack_Items_Second'Length = Stack_Items_First'Length
            and then (
                (for all I in 1..Stack_Items_Second'Last => Stack_Items_First(I) = Stack_Items_Second(I))
                and then
                (
                    for all I in 1..Stack_Items_First'Last =>
                         Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(Stack_Items_First)(I)
                         = Long_Natural(Size_of_Compact_Size(Input_Witness_Stack_Item_Size(Stack_Items_First(I))))
                           + Long_Natural(Input_Witness_Stack_Item_Size(Stack_Items_First(I)))
                           + (
                               if I = 1
                               then 0
                               else Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(Stack_Items_First)(I-1)
                           )
                )
                and then
                (
                    for all I in 1..Stack_Items_Second'Last =>
                         Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(Stack_Items_Second)(I)
                         = Long_Natural(Size_of_Compact_Size(Input_Witness_Stack_Item_Size(Stack_Items_Second(I))))
                           + Long_Natural(Input_Witness_Stack_Item_Size(Stack_Items_Second(I)))
                           + (
                               if I = 1
                               then 0
                               else Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(
                                       Stack_Items_Second
                                    )(I-1)
                             )
                )
            )
        ),
        Post => (
            (
                for all I in 1..Stack_Items_Second'Last =>
                    Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(Stack_Items_First)(I)
                        = Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(Stack_Items_Second)(I)
            )
            and then
            Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(Stack_Items_First)
                = Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(Stack_Items_Second)
        );

    package Witness_Data_Reader is new Data_Readers.Block_Reader(
        Octet_Type => Input_Witness_Data_Octet
    );

    function Input_Witness_Stack_Length(Stack: Input_Witness_Stack_Type) return Input_Witness_Stack_Length_Type
    with Post => Input_Witness_Stack_Length'Result <= Max_Witness_Stack_Items;

    function Input_Witness_Stack_Items(Stack: Input_Witness_Stack_Type)
        return Input_Witness_Stack_Items_Array_Type;

    function Input_Witness_Stack_Item(Stack: Input_Witness_Stack_Type; Item_Index: Input_Witness_Stack_Index_Type)
        return Input_Witness_Stack_Item_Type
    with Pre => Item_Index <= Input_Witness_Stack_Length(Stack);

    function Input_Witness_Stack_Item_Size(Item: Input_Witness_Stack_Item_Type) return Input_Witness_Data_Blob_Length_Type;

    function Input_Witness_Stack_Item_Data(Item: Input_Witness_Stack_Item_Type) return Input_Witness_Stack_Item_Data_Type;

    function Is_Input_Witness_Empty(Input_Witness: Input_Witness_Type) return Boolean is (
        Input_Witness_Stack_Length(Input_Witness.Stack) = 0
        and Scripts.Script_Length(Input_Witness.Script) = 0
    );

    function Input_Witness_Stack_Item_Max_Serialized_Data_Size return Natural
    is (Size_of_Compact_Size(Max_Witness_Data_Size) + Max_Witness_Data_Size)
    with Ghost;

    function Input_Witness_Stack_Item_Serialized_Data_Length(Stack_Item: Input_Witness_Stack_Item_Type) return Natural
    is (Size_of_Compact_Size(Input_Witness_Stack_Item_Size(Stack_Item)) + Input_Witness_Stack_Item_Size(Stack_Item))
    with Ghost => True,
         Post => (
             Input_Witness_Stack_Item_Serialized_Data_Length'Result <= Input_Witness_Stack_Item_Max_Serialized_Data_Size
         );

    function Input_Witness_Serialized_Data_Length(Witness: Input_Witness_Type) return Long_Natural
    is (
        if Is_Input_Witness_Empty(Witness) then 1
        else Long_Natural(
            Size_of_Compact_Size(Input_Witness_Stack_Length(Witness.Stack) + 1)
            + Natural(Input_Witness_Stack_Serialized_Data_Length(Input_Witness_Stack_Items(Witness.Stack)))
            + Size_of_Compact_Size(Scripts.Script_Length(Witness.Script))
            + Scripts.Script_Length(Witness.Script)
        )
    )
    with
        Ghost => True,
        Post => (
            Input_Witness_Serialized_Data_Length'Result
                <= Long_Natural(Input_Witness_Model_Max_Serialized_Data_Size)
        );


--    function Input_Witness_Stack_Items_Ghost_Tape_Match(
--        Stack_Items : Input_Witness_Stack_Items_Array_Type;
--        Offset      : Natural
--    ) return Boolean
--    with Ghost => True,
--         Pre => (
--             Stack_Items'First = 1
--             and then
--             Stack_Items'Last <= Max_Witness_Stack_Items
--             and then
--             Offset <= Natural'Last - Stack_Items'Length * Input_Witness_Stack_Item_Max_Serialized_Data_Size
--         ),
--         Global => (Input => Ghost_Data_Tape);

--    function Input_Witness_Stack_Item_Ghost_Tape_Match(Stack_Item: Input_Witness_Stack_Item_Type; Offset: Natural)
--    return Boolean
--    is (
--        Data_Readers.Compact_Data_Size_Ghost_Tape_Match(Input_Witness_Stack_Item_Size(Stack_Item), Offset)
--        and then (
--            if Input_Witness_Stack_Item_Size(Stack_Item) > 0 then (
--                Witness_Data_Reader.Block_Ghost_Tape_Match(
--                    Witness_Data_Reader.Block_Type(Input_Witness_Stack_Item_Data(Stack_Item)),
--                    Offset + Size_of_Compact_Size(Input_Witness_Stack_Item_Size(Stack_Item))
--                )
--            )
--        )
--    ) with Ghost => True,
--           Pre => Offset <= Natural'Last - Input_Witness_Stack_Item_Max_Serialized_Data_Size,
--           Global => (Input => Ghost_Data_Tape);

--    function Input_Witness_Ghost_Tape_Match(Input_Witness: Input_Witness_Type; Offset: Natural) return Boolean
--    is (
--        Data_Readers.Compact_Data_Size_Ghost_Tape_Match(Input_Witness_Stack_Length(Input_Witness.Stack), Offset)
--        and then
--        Input_Witness_Stack_Items_Ghost_Tape_Match(
--            Input_Witness_Stack_Items(Input_Witness.Stack),
--            Offset + Size_of_Compact_Size(Input_Witness_Stack_Length(Input_Witness.Stack))
--        )
--        and then
--        Scripts.Script_Ghost_Tape_Match(
--            Input_Witness.Script,
--            Offset + Size_of_Compact_Size(Input_Witness_Stack_Length(Input_Witness.Stack))
--            + Natural(Input_Witness_Stack_Serialized_Data_Length(Input_Witness_Stack_Items(Input_Witness.Stack)))
--        )
--    )
--    with Ghost => True,
--         Pre => Offset <= Natural'Last - Input_Witness_Model_Max_Serialized_Data_Size,
--         Global => (Input => Ghost_Data_Tape);

    procedure Deserialize(Input_Witness: out Input_Witness_Type)
    with
        Pre => (
            Status_OK
--            and then
--            Bytes_Processed <= Natural'Last - Input_Witness_Model_Max_Serialized_Data_Size
        ),
        Post => (
            if Status_OK then (
                Data_Checkpoints.Final_State_Consistent(Bytes_Processed_Since(Raw_Data_Status'Old))
                and then
                Data_Checkpoints.Element_Size_Match(
                    Tag_Input_Witness_Stack_Length,
                    Long_Natural(Size_of_Compact_Size(Input_Witness_Stack_Length(Input_Witness.Stack) + 1))
                )
--                and then
--                Data_Readers.Compact_Data_Size_Ghost_Tape_Match(
--                    (
--                        if Is_Input_Witness_Empty(Input_Witness) then 0
--                        else Input_Witness_Stack_Length(Input_Witness.Stack) + 1
--                    ),
--                    Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
--                        Tag_Input_Witness_Stack_Length,
--                        Data_Checkpoints.State
--                    )
--                )
                and then
                Data_Checkpoints.Element_Size_Match(
                    Tag_Input_Witness_Stack,
                    Input_Witness_Stack_Serialized_Data_Length(Input_Witness_Stack_Items(Input_Witness.Stack))
                )
--                and then
--                Input_Witness_Stack_Items_Ghost_Tape_Match(
--                    Input_Witness_Stack_Items(Input_Witness.Stack),
--                    Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
--                        Tag_Input_Witness_Stack,
--                        Data_Checkpoints.State
--                    )
--                )
                and then
                Data_Checkpoints.Element_Size_Match(
                    Tag_Input_Witness_Script,
                    (
                        if Is_Input_Witness_Empty(Input_Witness) then 0
                        else Long_Natural(
                            Size_of_Compact_Size(Scripts.Script_Length(Input_Witness.Script))
                                + Scripts.Script_Length(Input_Witness.Script)
                        )
                    )
                )
--                and then
--                (
--                    if not Is_Input_Witness_Empty(Input_Witness) then (
--                        Scripts.Script_Ghost_Tape_Match(
--                            Input_Witness.Script,
--                            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
--                                Tag_Input_Witness_Script,
--                                Data_Checkpoints.State
--                            )
--                        )
--                    )
--                )
--                and then
--                Input_Witness_Ghost_Tape_Match(Input_Witness, Bytes_Processed_Since(Raw_Data_Status'Old))
                and then
                Input_Witness_Serialized_Data_Length(Input_Witness)
                    = Long_Natural(Bytes_Processed_Since(Raw_Data_Status'Old))
            )
        ),
        Depends => (
            Input_Witness => (Raw_Data_Status, Structural_Status),
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

private

    subtype Witness_Data_Blob_Type is Input_Witness_Stack_Item_Data_Type (1..Max_Witness_Data_Size);
    subtype Input_Witness_Stack_Items_Bounded_Array_Type is
                Input_Witness_Stack_Items_Array_Type (Input_Witness_Stack_Index_Type);

    type Input_Witness_Stack_Item_Type is
        record
            Data_Length : Input_Witness_Data_Blob_Length_Type;
            Data        : Witness_Data_Blob_Type;
        end record;

    function Empty_Witness_Stack_Element return Input_Witness_Stack_Item_Type
    is ((Data_Length => 0, Data => (others => 0)));

    type Input_Witness_Stack_Type is
        record
            Length : Input_Witness_Stack_Length_Type;
            Items  : Input_Witness_Stack_Items_Bounded_Array_Type;
        end record;

    function Empty_Input_Witness_Stack return Input_Witness_Stack_Type
    is ((Length => 0, Items => (others => Empty_Witness_Stack_Element)));

    function Input_Witness_Stack_Length(Stack: Input_Witness_Stack_Type) return Input_Witness_Stack_Length_Type
    is (Stack.Length);

    function Input_Witness_Stack_Item(Stack: Input_Witness_Stack_Type; Item_Index: Input_Witness_Stack_Index_Type)
        return Input_Witness_Stack_Item_Type
    is (Stack.Items(Item_Index));

    function Input_Witness_Stack_Items(Stack: Input_Witness_Stack_Type)
        return Input_Witness_Stack_Items_Array_Type
    is (Stack.Items(1..Stack.Length));

    function Input_Witness_Stack_Item_Size(Item: Input_Witness_Stack_Item_Type) return Input_Witness_Data_Blob_Length_Type
    is (Item.Data_Length);

    function Input_Witness_Stack_Item_Data(Item: Input_Witness_Stack_Item_Type) return Input_Witness_Stack_Item_Data_Type 
    is (Item.Data(1..Item.Data_Length));

end Bitcoin_Like.Transaction_Input_Witnesses;
