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

package body Bitcoin_Like.Transaction_Input_Witnesses is

--    function Input_Witness_Stack_Items_Ghost_Tape_Match(
--        Stack_Items : Input_Witness_Stack_Items_Array_Type;
--        Offset      : Natural
--    ) return Boolean
--    is
--        Serialized_Length_Sums      : Serialized_Length_Cumulative_Sums_Type(Input_Witness_Stack_Index_Type) := (others => 0) with Ghost;
--        Serialized_Length_Sums_Prev : Serialized_Length_Cumulative_Sums_Type(Input_Witness_Stack_Index_Type) := (others => 0) with Ghost;
--    begin
--        if Stack_Items'Length = 0 then
--            return True;
--        end if;
--
--        pragma Assert (
--            Offset <= Natural'Last - Stack_Items'Length * Input_Witness_Stack_Item_Max_Serialized_Data_Size
--        );
--
--        for Index in Stack_Items'Range loop
--            if Index = 1 then
--                Serialized_Length_Sums(Index) := Long_Natural(
--                    Input_Witness_Stack_Item_Serialized_Data_Length(Stack_Items(Index)));
--            else
--                Serialized_Length_Sums(Index) := (
--                    Long_Natural(Input_Witness_Stack_Item_Serialized_Data_Length(Stack_Items(Index)))
--                        + Serialized_Length_Sums(Index - 1)
--                );
--            end if;
--
--            if not Input_Witness_Stack_Item_Ghost_Tape_Match(
--                        Stack_Items(Index),
--                        Offset + (if Index = 1 then 0 else Natural(Serialized_Length_Sums(Index - 1))))
--            then
--                return False;
--            end if;
--
--            pragma Loop_Invariant (
--                Serialized_Length_Sums = Serialized_Length_Sums_Prev'Update(
--                    Index => Serialized_Length_Sums(Index)
--                )
--            );
--            pragma Loop_Invariant(
--                for all I in 1..Index =>
--                    Serialized_Length_Sums(I) <= Long_Natural(Index * Input_Witness_Stack_Item_Max_Serialized_Data_Size)
--            );
--            pragma Loop_Invariant(
--                for all I in 1..Index => (
--                    Serialized_Length_Sums(I) = (
--                        Long_Natural(Input_Witness_Stack_Item_Serialized_Data_Length(Stack_Items(I)))
--                            + (if I = 1 then 0 else Serialized_Length_Sums(I - 1))
--                    )
--                )
--            );
--            pragma Loop_Invariant (
--                Input_Witness_Stack_Item_Ghost_Tape_Match(
--                        Stack_Items(Index),
--                        Offset + (if Index = 1 then 0 else Natural(Serialized_Length_Sums(Index - 1))))
--            );
--            pragma Loop_Invariant(
--                for all I in 1..Index => (
--                    Input_Witness_Stack_Item_Ghost_Tape_Match(
--                        Stack_Items(I),
--                        Offset + (if I = 1 then 0 else Natural(Serialized_Length_Sums(I - 1))))
--                )
--            );
--            Serialized_Length_Sums_Prev := Serialized_Length_Sums;
--        end loop;
--        return True;
--    end Input_Witness_Stack_Items_Ghost_Tape_Match;

    procedure Cumulative_Lengths_Eq_Lemma (
        Cumulative_Sums_First: Serialized_Length_Cumulative_Sums_Type;
        Cumulative_Sums_Second: Serialized_Length_Cumulative_Sums_Type;
        Stack_Items: Input_Witness_Stack_Items_Array_Type
    ) is begin
        for Index in 1..Stack_Items'Last loop
            if Index = 1 then
                pragma Assert (
                    Cumulative_Sums_First(1)
                        = Long_Natural(Size_of_Compact_Size(Input_Witness_Stack_Item_Size(Stack_Items(1))))
                            + Long_Natural(Input_Witness_Stack_Item_Size(Stack_Items(1)))
                );
                pragma Assert (
                    Cumulative_Sums_Second(1)
                        = Long_Natural(Size_of_Compact_Size(Input_Witness_Stack_Item_Size(Stack_Items(1))))
                            + Long_Natural(Input_Witness_Stack_Item_Size(Stack_Items(1)))
                );
                pragma Assert (Cumulative_Sums_First(1) = Cumulative_Sums_Second(1));
            else
                pragma Assert (Cumulative_Sums_First(Index - 1) = Cumulative_Sums_Second(Index - 1));
                pragma Assert (
                    Cumulative_Sums_First(Index)
                     = Long_Natural(Size_of_Compact_Size(Input_Witness_Stack_Item_Size(Stack_Items(Index))))
                        + Long_Natural(Input_Witness_Stack_Item_Size(Stack_Items(Index)))
                        + Cumulative_Sums_First(Index - 1)
                );
                pragma Assert (
                    Cumulative_Sums_Second(Index)
                     = Long_Natural(Size_of_Compact_Size(Input_Witness_Stack_Item_Size(Stack_Items(Index))))
                        + Long_Natural(Input_Witness_Stack_Item_Size(Stack_Items(Index)))
                        + Cumulative_Sums_Second(Index - 1)
                );
                pragma Assert (Cumulative_Sums_First(Index) = Cumulative_Sums_Second(Index));
            end if;
            pragma Loop_Invariant (
                for all I in Cumulative_Sums_First'First..Index =>
                    Cumulative_Sums_First(I) = Cumulative_Sums_Second(I)
            );
        end loop;
        pragma Assert (Cumulative_Sums_First = Cumulative_Sums_Second);
    end Cumulative_Lengths_Eq_Lemma;

    procedure Input_Witness_Stack_Serialized_Cumulative_Data_Lengths_Eq_Lemma(
        Stack_Items_First  : Input_Witness_Stack_Items_Array_Type;
        Stack_Items_Second : Input_Witness_Stack_Items_Array_Type
    )
    is
        Serialized_Sums_First: Serialized_Length_Cumulative_Sums_Type :=
            Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(Stack_Items_First);
        Serialized_Sums_Second: Serialized_Length_Cumulative_Sums_Type :=
            Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(Stack_Items_Second);
    begin
        Cumulative_Lengths_Eq_Lemma(
            Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(Stack_Items_First),
            Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(Stack_Items_Second),
            Stack_Items_First
        );
    end;

    function Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(
        Stack_Items: Input_Witness_Stack_Items_Array_Type
    )
    return Serialized_Length_Cumulative_Sums_Type
    is
        Item_Serialized_Length : Natural;
        Serialized_Length_Sums : Serialized_Length_Cumulative_Sums_Type (Stack_Items'Range) := (others => 0);
    begin
        for Index in Stack_Items'Range loop
            Item_Serialized_Length := Size_of_Compact_Size(Stack_Items(Index).Data_Length)
                                            + Stack_Items(Index).Data_Length;
            pragma Assert (
                Item_Serialized_Length
                        <= Size_of_Compact_Size(Max_Witness_Data_Size) + Max_Witness_Data_Size
            );
            pragma Assert (Item_Serialized_Length <= Natural'Last);

            Serialized_Length_Sums(Index) := Long_Natural(
                Item_Serialized_Length
                    + (if Index = 1 then 0 else Natural(Serialized_Length_Sums(Index - 1)))
            );

            pragma Loop_Invariant (
                for all I in 1..Index =>
                    Serialized_Length_Sums(I) <=
                        Long_Natural(I * (Size_of_Compact_Size(Max_Witness_Data_Size) + Max_Witness_Data_Size))
            );
            pragma Loop_Invariant (for all I in 1..Index => Serialized_Length_Sums(I) <= Long_Natural(Natural'Last));
            pragma Loop_Invariant (
                for all I in 1..Index =>
                     Serialized_Length_Sums(I)
                     = (if I = 1 then 0 else Serialized_Length_Sums(I-1))
                           + Long_Natural(Size_of_Compact_Size(Stack_Items(I).Data_Length))
                           + Long_Natural(Stack_Items(I).Data_Length)
            );
        end loop;
        return Serialized_Length_Sums;
    end;

    function Input_Witness_Stack_Serialized_Data_Length(
        Stack_Items: Input_Witness_Stack_Items_Array_Type
    ) return Long_Natural
    is begin
        if Stack_Items'Length = 0 then
            return 0;
        end if;

        return Long_Natural(
            Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(Stack_Items)(Stack_Items'Last)
        );
    end Input_Witness_Stack_Serialized_Data_Length;

    procedure Deserialize(Input_Witness: out Input_Witness_Type) is
        use Data_Checkpoints;

        Stack_Length : Compact_Size_Type;
        Data_Length  : Compact_Size_Type;

        Item_Serialized_Length        : Natural with Ghost;
        Raw_Data_Status_Old           : Raw_Data.Status_Type := Raw_Data_Status with Ghost;
        Bytes_Processed_at_Loop_Start : Natural with Ghost;
        Bytes_Processed_before_Stack  : Natural with Ghost;
        Bytes_Processed_within_Loop   : Natural := 0 with Ghost;
        Serialized_Length_Sums        : Serialized_Length_Cumulative_Sums_Type(Input_Witness_Stack_Index_Type) := (others => 0) with Ghost;
        Serialized_Length_Sums_Prev   : Serialized_Length_Cumulative_Sums_Type(Input_Witness_Stack_Index_Type) := (others => 0) with Ghost;
        Prev_Stack_Items              : Input_Witness_Stack_Items_Bounded_Array_Type with Ghost;
    begin
        -- Before we get 'Initialized attribute for arrays/records, we have to initialize
        -- beforehand, as we might not fill all the items, and if we fill the itmes in the loop,
        -- flow analysis currently cannot detect that all the items were actually initialized,
        -- unfortunately. We will reset Input_Witness to empty on all errors, though, anyway.
        Input_Witness := Empty_Input_Witness;

        Data_Checkpoint(Tag_Input_Witness_Stack_Length);

        Data_Readers.Read_Compact_Size(Stack_Length);

        if Got_Error then
            Input_Witness := Empty_Input_Witness;
            return;
        end if;

        Data_Checkpoint(Tag_Input_Witness_Stack);

        Bytes_Processed_before_Stack := Bytes_Processed;

        if Stack_Length = 0 then
            Input_Witness := Empty_Input_Witness;
            pragma Assert (Bytes_Processed_within_Loop = 0);
            pragma Assert (Input_Witness.Stack.Length = 0);
            pragma Assert (
                Bytes_Processed_within_Loop = Bytes_Processed - Bytes_Processed_before_Stack
            );
        elsif Stack_Length > 1 then
            if Stack_Length - 1 > Input_Witness.Stack.Items'Length then
                Register_Structural_Error(
                    "Stack length" & Natural'Image(Stack_Length)
                    & " is larger than maximum allowed length of" & Natural'Image(Input_Witness.Stack.Items'Length + 1)
                );
                Input_Witness := Empty_Input_Witness;
                return;
            end if;

            Prev_Stack_Items := Input_Witness.Stack.Items;

            for Index in 1..Stack_Length - 1 loop
                Bytes_Processed_at_Loop_Start := Bytes_Processed;

--                pragma Assert (
--                    Bytes_Processed_at_Loop_Start <= Natural'Last - Input_Witness_Stack_Item_Max_Serialized_Data_Size
--                );

                Data_Readers.Read_Compact_Size(Data_Length);

                if Got_Error then
                    Input_Witness := Empty_Input_Witness;
                    return;
                end if;

--                pragma Assert (
--                    Data_Readers.Compact_Data_Size_Ghost_Tape_Match(
--                        Data_Length, Bytes_Processed_at_Loop_Start)
--                );

                if Data_Length = 0 then
                    Input_Witness.Stack.Items(Index) := Empty_Witness_Stack_Element;
                    pragma Assert (Input_Witness.Stack.Items(Index).Data_Length = 0);
                else
                    if Data_Length > Max_Witness_Data_Size then
                        Register_Structural_Error(
                            "Witness stack item length" & Natural'Image(Data_Length)
                            & " is larger than maximum allowed length of"
                            & Natural'Image(Input_Witness.Stack.Items(Index).Data'Length)
                        );
                        Input_Witness := Empty_Input_Witness;
                        return;
                    end if;

                    Witness_Data_Reader.Read_Block_Zero_Fill(
                        Witness_Data_Reader.Block_Type(Input_Witness.Stack.Items(Index).Data),
                        Data_Length
                    );
                    if Got_Error then
                        Input_Witness := Empty_Input_Witness;
                        return;
                    end if;
                    Input_Witness.Stack.Items(Index).Data_Length := Data_Length;

--                    pragma Assert (
--                        Witness_Data_Reader.Block_Ghost_Tape_Match(
--                            Witness_Data_Reader.Block_Type(
--                                Input_Witness_Stack_Item_Data(Input_Witness.Stack.Items(Index))),
--                            Bytes_Processed_at_Loop_Start + Size_of_Compact_Size(Data_Length)
--                        )
--                    );
                end if;

--                pragma Assert (
--                    Input_Witness_Stack_Item_Ghost_Tape_Match(
--                        Input_Witness.Stack.Items(Index),
--                        Bytes_Processed_at_Loop_Start
--                    )
--                );

                Input_Witness.Stack.Length := Index;

                Item_Serialized_Length := Bytes_Processed - Bytes_Processed_at_Loop_Start;
                pragma Assert (Item_Serialized_Length > 0);
                pragma Assert (Item_Serialized_Length <= 9 + Max_Witness_Data_Size);
                pragma Assert (
                    Item_Serialized_Length =
                        Size_of_Compact_Size(Input_Witness.Stack.Items(Index).Data_Length)
                            + Input_Witness.Stack.Items(Index).Data_Length
                );
                pragma Assert (
                    Item_Serialized_Length =
                        Input_Witness_Stack_Item_Serialized_Data_Length(Input_Witness.Stack.Items(Index))
                );

                Bytes_Processed_within_Loop := Bytes_Processed_within_Loop + Item_Serialized_Length;
                Serialized_Length_Sums(Index) := Long_Natural(Bytes_Processed_within_Loop);

                pragma Loop_Invariant (Status_OK);
                pragma Loop_Invariant (
                    Serialized_Length_Sums = Serialized_Length_Sums_Prev'Update(
                        Index => Long_Natural(Bytes_Processed_within_Loop)
                    )
                );
                pragma Loop_Invariant (
                    Input_Witness.Stack.Items = Prev_Stack_Items'Update(
                        Index => Input_Witness.Stack.Items(Index)
                    )
                );
                pragma Loop_Invariant (
                    for all I in Ghost_Data_Tape'Range => (
                        if I < Raw_Data.Bytes_Processed(Raw_Data_Status'Loop_Entry)
                           and I >= Raw_Data.Bytes_Processed(Raw_Data_Status)
                        then Ghost_Data_Tape(I) = Ghost_Data_Tape'Loop_Entry(I)
                    )
                );
                pragma Loop_Invariant (Item_Serialized_Length > 0);
                pragma Loop_Invariant (
                    if Index = 1 then
                        Bytes_Processed_within_Loop = Item_Serialized_Length
                    else
                        Bytes_Processed_within_Loop =
                            Item_Serialized_Length + Natural(Serialized_Length_Sums(Index - 1))
                );
                pragma Loop_Invariant (
                    if Index = 1 then
                        Serialized_Length_Sums(Index) =
                            Long_Natural(
                                Input_Witness_Stack_Item_Serialized_Data_Length(Input_Witness.Stack.Items(Index)))
                    else
                        Serialized_Length_Sums(Index) =
                            Long_Natural(
                                Input_Witness_Stack_Item_Serialized_Data_Length(Input_Witness.Stack.Items(Index)))
                            + Serialized_Length_Sums(Index - 1)
                );
                pragma Loop_Invariant (
                    for all I in 1..Index =>
                        Serialized_Length_Sums(I) <= Long_Natural(I * Input_Witness_Stack_Item_Max_Serialized_Data_Size)
                );
                pragma Loop_Invariant (
                    for all I in 1..Index => (
                         Serialized_Length_Sums(I)
                         = Long_Natural(
                             Input_Witness_Stack_Item_Serialized_Data_Length(Input_Witness.Stack.Items(I)))
                           + (if I = 1 then 0 else Serialized_Length_Sums(I-1))
                     )
                );
--                pragma Loop_Invariant (
--                    Input_Witness_Stack_Item_Ghost_Tape_Match(
--                        Input_Witness.Stack.Items(Index),
--                        Bytes_Processed_before_Stack
--                            + (if Index = 1 then 0 else Natural(Serialized_Length_Sums(Index - 1))))
--                );
--                pragma Loop_Invariant (
--                    for all I in 1..Index =>
--                        Input_Witness_Stack_Item_Ghost_Tape_Match(
--                            Input_Witness.Stack.Items(I),
--                            Bytes_Processed_before_Stack
--                                + (if I = 1 then 0 else Natural(Serialized_Length_Sums(I - 1))))
--                );
--                pragma Loop_Invariant (
--                    Input_Witness_Stack_Items_Ghost_Tape_Match(
--                        Input_Witness.Stack.Items(1..Index),
--                        Bytes_Processed_before_Stack
--                    )
--                );
                pragma Loop_Invariant (
                   Input_Witness_Stack_Items(Input_Witness.Stack)'Last = Index
                );
                pragma Loop_Invariant (
                    Bytes_Processed_within_Loop = Bytes_Processed - Bytes_Processed_before_Stack
                );
--                pragma Loop_Invariant (
--                    Bytes_Processed <= Natural'Last - Input_Witness_Stack_Item_Max_Serialized_Data_Size
--                );

                Serialized_Length_Sums_Prev := Serialized_Length_Sums;
                Prev_Stack_Items := Input_Witness.Stack.Items;
            end loop;

            pragma Assert (
                Serialized_Length_Sums(Input_Witness_Stack_Items(Input_Witness.Stack)'Last)
                    = Long_Natural(Bytes_Processed_within_Loop)
            );

            Cumulative_Lengths_Eq_Lemma(
                Serialized_Length_Sums(1..Input_Witness_Stack_Items(Input_Witness.Stack)'Last),
                Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(Input_Witness_Stack_Items(Input_Witness.Stack)),
                Input_Witness_Stack_Items(Input_Witness.Stack)
            );

            pragma Assert (Bytes_Processed_within_Loop > 0);
            pragma Assert (Input_Witness.Stack.Length > 0);
            pragma Assert (
                Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(
                    Input_Witness_Stack_Items(Input_Witness.Stack)
                ) (Input_Witness_Stack_Items(Input_Witness.Stack)'Last)
                = Long_Natural(Bytes_Processed_within_Loop)
            );

--            pragma Assert (
--                Input_Witness_Stack_Items_Ghost_Tape_Match(
--                    Input_Witness_Stack_Items(Input_Witness.Stack),
--                    Bytes_Processed_before_Stack
--                )
--            );

        else
            pragma Assert (Bytes_Processed_within_Loop = 0);
            pragma Assert (Input_Witness_Stack_Items(Input_Witness.Stack)'Length = 0);
        end if;

        pragma Assert (
            Bytes_Processed_within_Loop = Bytes_Processed - Bytes_Processed_before_Stack
        );

        Data_Checkpoint(Tag_Input_Witness_Script);

        pragma Assert (
            Data_Checkpoints.Element_Size_Match(
                Tag_Input_Witness_Stack,
                Input_Witness_Stack_Serialized_Data_Length(Input_Witness_Stack_Items(Input_Witness.Stack))
            )
        );

        pragma Assert (
            if Input_Witness_Stack_Items(Input_Witness.Stack)'Length = 0
            then Bytes_Processed_within_Loop = 0
            else (
                Input_Witness_Stack_Serialized_Data_Cumulative_Lengths(
                    Input_Witness_Stack_Items(Input_Witness.Stack)
                ) (Input_Witness_Stack_Items(Input_Witness.Stack)'Last)
                = Long_Natural(Bytes_Processed_within_Loop)
            )
        );

        if Stack_Length > 0 then
            Scripts.Deserialize(Input_Witness.Script);

            if Got_Error then
                Input_Witness := Empty_Input_Witness;
                return;
            end if;

            -- Empty script cannot have TRUE on the stack after execution,
            -- and therefore will always fail
            if Scripts.Script_Length(Input_Witness.Script) = 0 then
                Register_Structural_Error("Witness script is empty");
                Input_Witness := Empty_Input_Witness;
                return;
            end if;
        end if;

        Data_Checkpoint_Final;

        if Stack_Length = 0 then
            pragma Assert (Data_Checkpoints.Element_Size_Match(Tag_Input_Witness_Stack_Length, 1));
            pragma Assert (Data_Checkpoints.Element_Size_Match(Tag_Input_Witness_Stack, 0));
            pragma Assert (Data_Checkpoints.Element_Size_Match(Tag_Input_Witness_Script, 0));
            pragma Assert (Is_Input_Witness_Empty(Input_Witness));
            pragma Assert (Data_Checkpoints.Element_Size_Match(Tag_Input_Witness_Script, 0));
        else
            pragma Assert (not Is_Input_Witness_Empty(Input_Witness));
            pragma Assert (
                Data_Checkpoints.Element_Size_Match(
                    Tag_Input_Witness_Script,
                    Long_Natural(
                            Size_of_Compact_Size(Scripts.Script_Length(Input_Witness.Script))
                                + Scripts.Script_Length(Input_Witness.Script)
                        )
                )
            );
        end if;

        pragma Assert (
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
        );

        pragma Assert(
            for all T in Input_Witness_Tag_Type'Range => Data_Checkpoints.Checkpoint_States.Tag_Seen(T, Data_Checkpoints.State)
        );
        pragma Assert(
             Input_Witness_Data_Model(Tag_Input_Witness_Stack_Length).From <= Data_Checkpoints.Checkpoint_States.Element_Size(Tag_Input_Witness_Stack_Length, Data_Checkpoints.State)
        );
        pragma Assert(
           Data_Checkpoints.Checkpoint_States.Element_Size(Tag_Input_Witness_Stack_Length, Data_Checkpoints.State) <= Input_Witness_Data_Model(Tag_Input_Witness_Stack_Length).To
        );
        pragma Assert(
             Input_Witness_Data_Model(Tag_Input_Witness_Stack).From <= Data_Checkpoints.Checkpoint_States.Element_Size(Tag_Input_Witness_Stack, Data_Checkpoints.State)
        );
        pragma Assert(
           Data_Checkpoints.Checkpoint_States.Element_Size(Tag_Input_Witness_Stack, Data_Checkpoints.State) <= Input_Witness_Data_Model(Tag_Input_Witness_Stack).To
        );
        pragma Assert(
             Input_Witness_Data_Model(Tag_Input_Witness_Script).From <= Data_Checkpoints.Checkpoint_States.Element_Size(Tag_Input_Witness_Script, Data_Checkpoints.State)
        );
        pragma Assert(
           Data_Checkpoints.Checkpoint_States.Element_Size(Tag_Input_Witness_Script, Data_Checkpoints.State) <= Input_Witness_Data_Model(Tag_Input_Witness_Script).To
        );
        pragma Assert (Data_Checkpoints.Final_State_Consistent(Bytes_Processed_Since(Raw_Data_Status_Old)));

        pragma Assert (
            Data_Checkpoints.Element_Size_Match(
                Tag_Input_Witness_Stack,
                Input_Witness_Stack_Serialized_Data_Length(Input_Witness_Stack_Items(Input_Witness.Stack))
            )
        );
        pragma Assert (
            Input_Witness_Serialized_Data_Length(Input_Witness)
                = Long_Natural(Bytes_Processed_Since(Raw_Data_Status_Old))
        );

    end Deserialize;
end Bitcoin_Like.Transaction_Input_Witnesses;

