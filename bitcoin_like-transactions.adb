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

package body Bitcoin_Like.Transactions is

function Is_Transaction_Input_Witnesses_Empty(Input_Witnesses: Transaction_Input_Witnesses_Array_Type) return Boolean is
begin
    for Index in Input_Witnesses'Range loop
        if not Is_Input_Witness_Empty(Input_Witnesses(Index)) then
            return False;
        end if;
        pragma Loop_Invariant(
            for all I in Input_Witnesses'First..Index
                => Is_Input_Witness_Empty(Input_Witnesses(I)));
    end loop;
    return True;
end Is_Transaction_Input_Witnesses_Empty;

procedure Inputs_Serialized_Cumulative_Data_Lengths_Eq_Lemma(
        Inputs_Array_First  : Transaction_Inputs_Array_Type;
        Inputs_Array_Second : Transaction_Inputs_Array_Type
)
is
    Serialized_Sums_First: Inputs_Serialized_Cumulative_Sums_Type :=
        Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_First);
    Serialized_Sums_Second: Inputs_Serialized_Cumulative_Sums_Type :=
        Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_Second);
begin
    pragma Assert (
        Serialized_Sums_First'Length = Serialized_Sums_Second'Length + 1
        or
        Serialized_Sums_First'Length = Serialized_Sums_Second'Length
    );

    for Index in 0..Serialized_Sums_Second'Last loop
        if Index = 0 then
            pragma Assert (
                Serialized_Sums_First(Index) = Inputs.Input_Serialized_Data_Length(Inputs_Array_First(Index))
            );
            pragma Assert (
                Serialized_Sums_Second(Index) = Inputs.Input_Serialized_Data_Length(Inputs_Array_First(Index))
            );
            pragma Assert (Serialized_Sums_First(Index) = Serialized_Sums_Second(Index));
        else
            pragma Assert (Serialized_Sums_First(Index - 1) = Serialized_Sums_Second(Index - 1));
            pragma Assert (
                Serialized_Sums_First(Index) =
                    Inputs.Input_Serialized_Data_Length(Inputs_Array_First(Index))
                    + Serialized_Sums_First(Index - 1)
            );
            pragma Assert (
                Serialized_Sums_Second(Index) =
                    Inputs.Input_Serialized_Data_Length(Inputs_Array_First(Index))
                    + Serialized_Sums_First(Index - 1)
            );
            pragma Assert (Serialized_Sums_First(Index) = Serialized_Sums_Second(Index));
        end if;
        pragma Loop_Invariant (
            for all I in Serialized_Sums_First'First..Index =>
                Serialized_Sums_First(I) = Serialized_Sums_Second(I)
        );
    end loop;
    pragma Assert (
        for all I in 0..Serialized_Sums_Second'Last =>
            Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_First)(I)
                = Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_Second)(I)
    );
    pragma Assert (
        if Inputs_Array_First'Length = Inputs_Array_Second'Length
        then Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_First)(Inputs_Array_First'Last)
                = Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_Second)(Inputs_Array_Second'Last)
        else Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_First)(Inputs_Array_First'Last)
                = Inputs.Input_Serialized_Data_Length(Inputs_Array_First(Inputs_Array_First'Last))
                    + Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array_Second)(Inputs_Array_Second'Last)
    );
end;

function Inputs_Serialized_Cumulative_Data_Lengths(Inputs_Array: Transaction_Inputs_Array_Type)
    return Inputs_Serialized_Cumulative_Sums_Type
is
    Cumulative_Sizes: Inputs_Serialized_Cumulative_Sums_Type (Inputs_Array'Range) := (others => 0);
    Element_Size: Long_Natural;
begin
    pragma Assert (Inputs_Array'First = 0);
    for Index in 0..Inputs_Array'Last loop

        Element_Size := Inputs.Input_Serialized_Data_Length(Inputs_Array(Index));
        pragma Assert (Element_Size <= Long_Natural(Inputs.Input_Model_Max_Serialized_Data_Size));

        Cumulative_Sizes(Index) :=
            Element_Size
                + (
                    if Index = 0
                    then 0
                    else Cumulative_Sizes(Index - 1)
                  );

        pragma Loop_Invariant(
            Cumulative_Sizes(Index) <=
                Long_Natural(Inputs.Input_Model_Max_Serialized_Data_Size)
                    * Long_Natural(Index + 1)
        );
        pragma Loop_Invariant(
            for all I in 0..Index =>
                Cumulative_Sizes(I) <=
                    Long_Natural(Inputs.Input_Model_Max_Serialized_Data_Size)
                        * Long_Natural(I + 1)
        );
        pragma Loop_Invariant(
            for all I in 0..Index =>
                Cumulative_Sizes(I) =
                    Inputs.Input_Serialized_Data_Length(Inputs_Array(I))
                           + (
                                if I = 0
                                then 0
                                else Cumulative_Sizes(I - 1)
                             )
        );
    end loop;

    pragma Assert (
        if Inputs_Array'Length > 0
        then Cumulative_Sizes(Inputs_Array'Last)
                <= Long_Natural(Inputs.Input_Model_Max_Serialized_Data_Size) * Inputs_Array'Length
    );
    return Cumulative_Sizes;
end Inputs_Serialized_Cumulative_Data_Lengths;

procedure Outputs_Serialized_Cumulative_Data_Lengths_Eq_Lemma(
        Outputs_Array_First  : Transaction_Outputs_Array_Type;
        Outputs_Array_Second : Transaction_Outputs_Array_Type
)
is
    Serialized_Sums_First: Outputs_Serialized_Cumulative_Sums_Type :=
        Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array_First);
    Serialized_Sums_Second: Outputs_Serialized_Cumulative_Sums_Type :=
        Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array_Second);
begin
    pragma Assert (Serialized_Sums_First'Length = Serialized_Sums_Second'Length + 1);

    for Index in 0..Serialized_Sums_Second'Last loop
        if Index = 0 then
            pragma Assert (
                Serialized_Sums_First(Index) = Outputs.Output_Serialized_Data_Length(Outputs_Array_First(Index))
            );
            pragma Assert (
                Serialized_Sums_Second(Index) = Outputs.Output_Serialized_Data_Length(Outputs_Array_First(Index))
            );
            pragma Assert (Serialized_Sums_First(Index) = Serialized_Sums_Second(Index));
        else
            pragma Assert (Serialized_Sums_First(Index - 1) = Serialized_Sums_Second(Index - 1));
            pragma Assert (
                Serialized_Sums_First(Index) =
                    Outputs.Output_Serialized_Data_Length(Outputs_Array_First(Index))
                    + Serialized_Sums_First(Index - 1)
            );
            pragma Assert (
                Serialized_Sums_Second(Index) =
                    Outputs.Output_Serialized_Data_Length(Outputs_Array_First(Index))
                    + Serialized_Sums_First(Index - 1)
            );
            pragma Assert (Serialized_Sums_First(Index) = Serialized_Sums_Second(Index));
        end if;
        pragma Loop_Invariant (
            for all I in Serialized_Sums_First'First..Index =>
                Serialized_Sums_First(I) = Serialized_Sums_Second(I)
        );
    end loop;
    pragma Assert (
        for all I in 0..Serialized_Sums_Second'Last =>
            Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array_First)(I)
                = Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array_Second)(I)
    );
    pragma Assert (
        Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array_First)(Outputs_Array_First'Last)
            = Outputs.Output_Serialized_Data_Length(Outputs_Array_First(Outputs_Array_First'Last))
                + Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array_Second)(Outputs_Array_Second'Last)
    );
end;

function Outputs_Serialized_Cumulative_Data_Lengths(Outputs_Array: Transaction_Outputs_Array_Type)
    return Outputs_Serialized_Cumulative_Sums_Type
is
    Cumulative_Sizes: Outputs_Serialized_Cumulative_Sums_Type (Outputs_Array'Range) := (others => 0);
    Element_Size: Long_Natural;
begin
    pragma Assert (Outputs_Array'First = 0);
    for Index in 0..Outputs_Array'Last loop

        Element_Size := Outputs.Output_Serialized_Data_Length(Outputs_Array(Index));
        pragma Assert (Element_Size <= Long_Natural(Outputs.Output_Model_Max_Serialized_Data_Size));

        Cumulative_Sizes(Index) :=
            Element_Size
                + (
                    if Index = 0
                    then 0
                    else Cumulative_Sizes(Index - 1)
                  );

        pragma Loop_Invariant(
            Cumulative_Sizes(Index) <=
                Long_Natural(Outputs.Output_Model_Max_Serialized_Data_Size)
                    * Long_Natural(Index + 1)
        );
        pragma Loop_Invariant(
            for all I in 0..Index =>
                Cumulative_Sizes(I) <=
                    Long_Natural(Outputs.Output_Model_Max_Serialized_Data_Size)
                        * Long_Natural(I + 1)
        );
        pragma Loop_Invariant(
            for all I in 0..Index =>
                Cumulative_Sizes(I) =
                    Outputs.Output_Serialized_Data_Length(Outputs_Array(I))
                           + (
                                if I = 0
                                then 0
                                else Cumulative_Sizes(I - 1)
                             )
        );
    end loop;

    pragma Assert (
        if Outputs_Array'Length > 0
        then Cumulative_Sizes(Outputs_Array'Last)
                <= Long_Natural(Outputs.Output_Model_Max_Serialized_Data_Size) * Outputs_Array'Length
    );
    return Cumulative_Sizes;
end Outputs_Serialized_Cumulative_Data_Lengths;

procedure Input_Witnesses_Serialized_Cumulative_Data_Lengths_Eq_Lemma(
        Input_Witnesses_Array_First  : Transaction_Input_Witnesses_Array_Type;
        Input_Witnesses_Array_Second : Transaction_Input_Witnesses_Array_Type
)
is
    Serialized_Sums_First: Input_Witnesses_Serialized_Cumulative_Sums_Type :=
        Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array_First);
    Serialized_Sums_Second: Input_Witnesses_Serialized_Cumulative_Sums_Type :=
        Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array_Second);
begin
    pragma Assert (Serialized_Sums_First'Length = Serialized_Sums_Second'Length + 1);

    for Index in 0..Serialized_Sums_Second'Last loop
        if Index = 0 then
            pragma Assert (
                Input_Witnesses_Array_First(Index) = Input_Witnesses_Array_Second(Index)
            );
            Input_Witness_Stack_Serialized_Cumulative_Data_Lengths_Eq_Lemma(
                Input_Witness_Stack_Items(Input_Witnesses_Array_First(Index).Stack),
                Input_Witness_Stack_Items(Input_Witnesses_Array_Second(Index).Stack)
            );
            pragma Assert (
                Input_Witnesses.Input_Witness_Serialized_Data_Length(Input_Witnesses_Array_First(0))
                        = Input_Witnesses.Input_Witness_Serialized_Data_Length(Input_Witnesses_Array_Second(0))
            );
            pragma Assert (
                Serialized_Sums_First(Index) = Input_Witnesses.Input_Witness_Serialized_Data_Length(Input_Witnesses_Array_First(Index))
            );
            pragma Assert (
                Serialized_Sums_Second(Index) = Input_Witnesses.Input_Witness_Serialized_Data_Length(Input_Witnesses_Array_First(Index))
            );
            pragma Assert (Serialized_Sums_First(Index) = Serialized_Sums_Second(Index));
        else
            pragma Assert (Serialized_Sums_First(Index - 1) = Serialized_Sums_Second(Index - 1));
            Input_Witness_Stack_Serialized_Cumulative_Data_Lengths_Eq_Lemma(
                Input_Witness_Stack_Items(Input_Witnesses_Array_First(Index).Stack),
                Input_Witness_Stack_Items(Input_Witnesses_Array_Second(Index).Stack)
            );
            pragma Assert (
                Serialized_Sums_First(Index) =
                    Input_Witnesses.Input_Witness_Serialized_Data_Length(Input_Witnesses_Array_First(Index))
                    + Serialized_Sums_First(Index - 1)
            );
            pragma Assert (
                Serialized_Sums_Second(Index) =
                    Input_Witnesses.Input_Witness_Serialized_Data_Length(Input_Witnesses_Array_First(Index))
                    + Serialized_Sums_First(Index - 1)
            );
            pragma Assert (Serialized_Sums_First(Index) = Serialized_Sums_Second(Index));
        end if;
        pragma Loop_Invariant (
            for all I in Serialized_Sums_First'First..Index =>
                Serialized_Sums_First(I) = Serialized_Sums_Second(I)
        );
    end loop;
    pragma Assert (
        for all I in 0..Serialized_Sums_Second'Last =>
            Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array_First)(I)
                = Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array_Second)(I)
    );
    pragma Assert (
        Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array_First)(Input_Witnesses_Array_First'Last)
            = Input_Witnesses.Input_Witness_Serialized_Data_Length(Input_Witnesses_Array_First(Input_Witnesses_Array_First'Last))
                + Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array_Second)(Input_Witnesses_Array_Second'Last)
    );
end;

function Input_Witnesses_Serialized_Cumulative_Data_Lengths(Input_Witnesses_Array: Transaction_Input_Witnesses_Array_Type)
    return Input_Witnesses_Serialized_Cumulative_Sums_Type
is
    Cumulative_Sizes: Input_Witnesses_Serialized_Cumulative_Sums_Type (Input_Witnesses_Array'Range) := (others => 0);
    Element_Size: Long_Natural;
begin
    pragma Assert (Input_Witnesses_Array'First = 0);
    for Index in 0..Input_Witnesses_Array'Last loop

        Element_Size := Input_Witnesses.Input_Witness_Serialized_Data_Length(Input_Witnesses_Array(Index));
        pragma Assert (Element_Size <= Long_Natural(Input_Witnesses.Input_Witness_Model_Max_Serialized_Data_Size));

        Cumulative_Sizes(Index) :=
            Element_Size
                + (
                    if Index = 0
                    then 0
                    else Cumulative_Sizes(Index - 1)
                  );

        pragma Loop_Invariant(
            Cumulative_Sizes(Index) <=
                Long_Natural(Input_Witnesses.Input_Witness_Model_Max_Serialized_Data_Size)
                    * Long_Natural(Index + 1)
        );
        pragma Loop_Invariant(
            for all I in 0..Index =>
                Cumulative_Sizes(I) <=
                    Long_Natural(Input_Witnesses.Input_Witness_Model_Max_Serialized_Data_Size)
                        * Long_Natural(I + 1)
        );
        pragma Loop_Invariant(
            for all I in 0..Index =>
                Cumulative_Sizes(I) =
                    Input_Witnesses.Input_Witness_Serialized_Data_Length(Input_Witnesses_Array(I))
                           + (
                                if I = 0
                                then 0
                                else Cumulative_Sizes(I - 1)
                             )
        );
    end loop;

    pragma Assert (
        if Input_Witnesses_Array'Length > 0
        then Cumulative_Sizes(Input_Witnesses_Array'Last)
                <= Long_Natural(Input_Witnesses.Input_Witness_Model_Max_Serialized_Data_Size) * Input_Witnesses_Array'Length
    );
    return Cumulative_Sizes;
end Input_Witnesses_Serialized_Cumulative_Data_Lengths;

procedure Deserialize_Inputs(Transaction: in out Transaction_Type; Inputs_Count: in Input_Array_Length_Type)
is
    subtype Serialized_Length_Cumulative_Sums_Type is Inputs_Serialized_Cumulative_Sums_Type (Input_Index_Type);

    Bytes_Processed_at_Loop_Start      : Long_Natural with Ghost;
    Bytes_Processed_before_Loop        : Long_Natural := Long_Natural(Bytes_Processed) with Ghost;
    Bytes_Processed_within_Loop        : Long_Natural := 0 with Ghost;
    Item_Serialized_Length             : Long_Natural with Ghost;
    Serialized_Cumulative_Data_Lengths : Serialized_Length_Cumulative_Sums_Type := (others => 0) with Ghost;
    Transaction_Prev                   : Transaction_Type := Transaction with Ghost;

    Saved_Data_Tag : Model_Tag_Type := Current_Data_Tag;
begin
    Transaction.Inputs_Count := 0;

    if Inputs_Count = 0 then
        return;
    end if;

    for Index in 0..Integer(Inputs_Count)-1 loop

        Bytes_Processed_at_Loop_Start := Long_Natural(Bytes_Processed);

        Inputs.Deserialize(Transaction.Inputs(Index));

        if Got_Error then
            return; -- caller will check the error and set transaction to empty
        end if;

        Transaction.Inputs_Count := Index + 1;

        pragma Assert (Transaction_Inputs(Transaction)'Length = Index + 1);
        pragma Assert (Transaction_Inputs(Transaction)'Last = Index);

        pragma Assert (
            Transaction.Inputs = Transaction_Prev.Inputs'Update(
                Index => Transaction.Inputs(Index)
            )
        );

        Item_Serialized_Length := Long_Natural(Bytes_Processed) - Bytes_Processed_at_Loop_Start;
        pragma Assert (Item_Serialized_Length > 0);

        Bytes_Processed_within_Loop := Bytes_Processed_within_Loop + Item_Serialized_Length;

        pragma Assert (Bytes_Processed_within_Loop > 0);
        pragma Assert (Item_Serialized_Length <= Long_Natural(Inputs.Input_Model_Max_Serialized_Data_Size));
        pragma Assert (
            Inputs.Input_Serialized_Data_Length(Transaction.Inputs(Index))
                = Item_Serialized_Length
        );

        Serialized_Cumulative_Data_Lengths(Index) :=
            Item_Serialized_Length + (
                                        if Index = 0
                                        then 0
                                        else Serialized_Cumulative_Data_Lengths(Index - 1)
                                     );

        if Index = 0 then
            pragma Assert (
                Serialized_Cumulative_Data_Lengths(Index)
                    = Inputs.Input_Serialized_Data_Length(Transaction_Inputs(Transaction)(Index))
            );
            pragma Assert (
                Inputs_Serialized_Cumulative_Data_Lengths(Transaction_Inputs(Transaction))(Index)
                    = Inputs.Input_Serialized_Data_Length(Transaction_Inputs(Transaction)(Index))
            );
            pragma Assert (
                Serialized_Cumulative_Data_Lengths(Index)
                    = Inputs_Serialized_Cumulative_Data_Lengths(Transaction.Inputs(0..Index))(Index)
            );
        else
            Inputs_Serialized_Cumulative_Data_Lengths_Eq_Lemma(
                Transaction.Inputs(0..Index), Transaction_Prev.Inputs(0..Index-1)
            );
            pragma Assert (
                for all I in 0..Index-1 =>
                   Inputs_Serialized_Cumulative_Data_Lengths(Transaction.Inputs(0..Index))(I)
                        = Inputs_Serialized_Cumulative_Data_Lengths(Transaction_Prev.Inputs(0..Index-1))(I)
            );
            pragma Assert (
                Serialized_Cumulative_Data_Lengths(Index - 1)
                    = Inputs_Serialized_Cumulative_Data_Lengths(Transaction.Inputs(0..Index))(Index - 1)
            );
            pragma Assert (
                Serialized_Cumulative_Data_Lengths(Index)
                    = Inputs.Input_Serialized_Data_Length(Transaction.Inputs(Index))
                    + Serialized_Cumulative_Data_Lengths(Index - 1)
            );
            pragma Assert (
                Inputs_Serialized_Cumulative_Data_Lengths(Transaction.Inputs(0..Index))(Index)
                    = Inputs.Input_Serialized_Data_Length(Transaction.Inputs(Index))
                        + Inputs_Serialized_Cumulative_Data_Lengths(Transaction.Inputs(0..Index))(Index - 1)
            );
            pragma Assert (
                Serialized_Cumulative_Data_Lengths(Index)
                    = Inputs_Serialized_Cumulative_Data_Lengths(Transaction.Inputs(0..Index))(Index)
            );
        end if;
        pragma Loop_Invariant (
            Serialized_Cumulative_Data_Lengths(Index)
                = Inputs_Serialized_Cumulative_Data_Lengths(Transaction.Inputs(0..Index))(Index)
        );
        pragma Loop_Invariant (
            for all I in 0..Index =>
                Serialized_Cumulative_Data_Lengths(I)
                    = Inputs_Serialized_Cumulative_Data_Lengths(Transaction.Inputs(0..Index))(I)
        );
        pragma Loop_Invariant (Status_OK);
        pragma Loop_Invariant (Bytes_Processed_within_Loop > 0);
        pragma Loop_Invariant (Transaction_Inputs(Transaction)'Length = Index + 1);
        pragma Loop_Invariant(
            Serialized_Cumulative_Data_Lengths(Index) <=
                Long_Natural(Inputs.Input_Model_Max_Serialized_Data_Size)
                    * Long_Natural(Index + 1)
        );
        pragma Loop_Invariant(
            for all I in 0..Index =>
                Serialized_Cumulative_Data_Lengths(I) <=
                    Long_Natural(Inputs.Input_Model_Max_Serialized_Data_Size)
                        * Long_Natural(I + 1)
        );
        pragma Loop_Invariant (
            if Index = 0 then
                Bytes_Processed_within_Loop = Item_Serialized_Length
            else
                Bytes_Processed_within_Loop =
                    Item_Serialized_Length + Serialized_Cumulative_Data_Lengths(Index - 1)
        );
        pragma Loop_Invariant (
            Bytes_Processed_within_Loop = Serialized_Cumulative_Data_Lengths(Index)
        );
        pragma Loop_Invariant(
            Bytes_Processed_within_Loop
                = Long_Natural(Bytes_Processed) - Bytes_Processed_before_Loop
        );
        Transaction_Prev := Transaction;
    end loop;
    pragma Assert (Bytes_Processed_within_Loop > 0);
    pragma Assert (Bytes_Processed_within_Loop = Long_Natural(Bytes_Processed) - Bytes_Processed_before_Loop);
    pragma Assert (Transaction_Inputs(Transaction)'Last = Inputs_Count - 1);
    pragma Assert (Transaction_Inputs(Transaction)'Length = Inputs_Count);
    pragma Assert (
        Inputs_Serialized_Cumulative_Data_Lengths(
           Transaction_Inputs(Transaction)
        )(Transaction_Inputs(Transaction)'Last)
        = Serialized_Cumulative_Data_Lengths(Transaction_Inputs(Transaction)'Last)
    );
    pragma Assert (Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction)) = Bytes_Processed_within_Loop);

    Data_Checkpointing.Force_Set_Data_Tag(Saved_Data_Tag, Data_Tag_State);

    pragma Assert_And_Cut (
        Status_OK
        and then
        Transaction_Inputs_Count(Transaction) = Inputs_Count
        and then
        Current_Data_Tag = Saved_Data_Tag
        and then
        Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction))
            = Long_Natural(Bytes_Processed) - Bytes_Processed_before_Loop
        and then
        Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction))
            <= Long_Natural(Inputs.Input_Model_Max_Serialized_Data_Size) * Long_Natural(Inputs_Count)
    );
end Deserialize_Inputs;

procedure Deserialize_Outputs(Transaction: in out Transaction_Type; Outputs_Count: in Output_Array_Length_Type)
is
    subtype Serialized_Length_Cumulative_Sums_Type is Outputs_Serialized_Cumulative_Sums_Type (Output_Index_Type);

    Bytes_Processed_at_Loop_Start      : Long_Natural with Ghost;
    Bytes_Processed_within_Loop        : Long_Natural := 0 with Ghost;
    Item_Serialized_Length             : Long_Natural with Ghost;
    Serialized_Cumulative_Data_Lengths : Serialized_Length_Cumulative_Sums_Type := (others => 0) with Ghost;
    Transaction_Prev                   : Transaction_Type := Transaction with Ghost;
    Transaction_Old                    : Transaction_Type := Transaction with Ghost;
    Raw_Data_Status_Old                : Raw_Data.Status_Type := Raw_Data_Status with Ghost;

    Saved_Data_Tag : Model_Tag_Type := Current_Data_Tag;
begin
    Transaction.Outputs_Count := 0;

    if Outputs_Count = 0 then
        return;
    end if;

    for Index in 0..Integer(Outputs_Count)-1 loop

        Bytes_Processed_at_Loop_Start := Long_Natural(Bytes_Processed);

        Outputs.Deserialize(Transaction.Outputs(Index));

        if Got_Error then
            return; -- caller will check the error and set transaction to empty
        end if;

        Transaction.Outputs_Count := Index + 1;

        pragma Assert (Transaction_Outputs(Transaction)'Length = Index + 1);
        pragma Assert (Transaction_Outputs(Transaction)'Last = Index);

        pragma Assert (
            Transaction = Transaction_Old'Update(
                Outputs_Count => Index + 1,
                Outputs => Transaction_Prev.Outputs'Update(
                    Index => Transaction.Outputs(Index)
                )
            )
        );

        Item_Serialized_Length := Long_Natural(Bytes_Processed) - Bytes_Processed_at_Loop_Start;
        pragma Assert (Item_Serialized_Length > 0);

        Bytes_Processed_within_Loop := Bytes_Processed_within_Loop + Item_Serialized_Length;

        pragma Assert (Bytes_Processed_within_Loop > 0);
        pragma Assert (Item_Serialized_Length <= Long_Natural(Outputs.Output_Model_Max_Serialized_Data_Size));
        pragma Assert (
            Outputs.Output_Serialized_Data_Length(Transaction.Outputs(Index))
                = Item_Serialized_Length
        );

        Serialized_Cumulative_Data_Lengths(Index) :=
            Item_Serialized_Length + (
                                        if Index = 0
                                        then 0
                                        else Serialized_Cumulative_Data_Lengths(Index - 1)
                                     );

        if Index = 0 then
            pragma Assert (
                Serialized_Cumulative_Data_Lengths(Index)
                    = Outputs.Output_Serialized_Data_Length(Transaction_Outputs(Transaction)(Index))
            );
            pragma Assert (
                Outputs_Serialized_Cumulative_Data_Lengths(Transaction_Outputs(Transaction))(Index)
                    = Outputs.Output_Serialized_Data_Length(Transaction_Outputs(Transaction)(Index))
            );
            pragma Assert (
                Serialized_Cumulative_Data_Lengths(Index)
                    = Outputs_Serialized_Cumulative_Data_Lengths(Transaction.Outputs(0..Index))(Index)
            );
        else
            Outputs_Serialized_Cumulative_Data_Lengths_Eq_Lemma(
                Transaction.Outputs(0..Index), Transaction_Prev.Outputs(0..Index-1)
            );
            pragma Assert (
                for all I in 0..Index-1 =>
                   Outputs_Serialized_Cumulative_Data_Lengths(Transaction.Outputs(0..Index))(I)
                        = Outputs_Serialized_Cumulative_Data_Lengths(Transaction_Prev.Outputs(0..Index-1))(I)
            );
            pragma Assert (
                Serialized_Cumulative_Data_Lengths(Index - 1)
                    = Outputs_Serialized_Cumulative_Data_Lengths(Transaction.Outputs(0..Index))(Index - 1)
            );
            pragma Assert (
                Serialized_Cumulative_Data_Lengths(Index)
                    = Outputs.Output_Serialized_Data_Length(Transaction.Outputs(Index))
                    + Serialized_Cumulative_Data_Lengths(Index - 1)
            );
            pragma Assert (
                Outputs_Serialized_Cumulative_Data_Lengths(Transaction.Outputs(0..Index))(Index)
                    = Outputs.Output_Serialized_Data_Length(Transaction.Outputs(Index))
                        + Outputs_Serialized_Cumulative_Data_Lengths(Transaction.Outputs(0..Index))(Index - 1)
            );
            pragma Assert (
                Serialized_Cumulative_Data_Lengths(Index)
                    = Outputs_Serialized_Cumulative_Data_Lengths(Transaction.Outputs(0..Index))(Index)
            );
        end if;
        pragma Loop_Invariant (
            Serialized_Cumulative_Data_Lengths(Index)
                = Outputs_Serialized_Cumulative_Data_Lengths(Transaction.Outputs(0..Index))(Index)
        );
        pragma Loop_Invariant (
            for all I in 0..Index =>
                Serialized_Cumulative_Data_Lengths(I)
                    = Outputs_Serialized_Cumulative_Data_Lengths(Transaction.Outputs(0..Index))(I)
        );
        pragma Loop_Invariant (Status_OK);
        pragma Loop_Invariant (Bytes_Processed_within_Loop > 0);
        pragma Loop_Invariant (Transaction_Outputs(Transaction)'Last = Index);
        pragma Loop_Invariant(
            Serialized_Cumulative_Data_Lengths(Index) <=
                Long_Natural(Outputs.Output_Model_Max_Serialized_Data_Size)
                    * Long_Natural(Index + 1)
        );
        pragma Loop_Invariant(
            for all I in 0..Index =>
                Serialized_Cumulative_Data_Lengths(I) <=
                    Long_Natural(Outputs.Output_Model_Max_Serialized_Data_Size)
                        * Long_Natural(I + 1)
        );
        pragma Loop_Invariant (
            if Index = 0 then
                Bytes_Processed_within_Loop = Item_Serialized_Length
            else
                Bytes_Processed_within_Loop =
                    Item_Serialized_Length + Serialized_Cumulative_Data_Lengths(Index - 1)
        );
        pragma Loop_Invariant (
            Bytes_Processed_within_Loop = Serialized_Cumulative_Data_Lengths(Index)
        );
        pragma Loop_Invariant(
            Bytes_Processed_within_Loop = Long_Natural(Bytes_Processed_Since(Raw_Data_Status_Old))
        );
        pragma Loop_Invariant (
            Transaction = Transaction_Old'Update(
                Outputs_Count => Index + 1,
                Outputs => Transaction_Prev.Outputs'Update(
                    Index => Transaction.Outputs(Index)
                )
            )
        );
        pragma Loop_Invariant (
            Transaction_Version(Transaction) = Transaction_Version(Transaction_Old)
            and then
            Transaction_Inputs_Count(Transaction) = Transaction_Inputs_Count(Transaction_Old)
            and then
            Transaction_Inputs(Transaction) = Transaction_Inputs(Transaction_Old)
        );
        Transaction_Prev := Transaction;
    end loop;
    pragma Assert (Bytes_Processed_within_Loop > 0);
    pragma Assert (Bytes_Processed_within_Loop = Long_Natural(Bytes_Processed_Since(Raw_Data_Status_Old)));
    pragma Assert (Transaction.Outputs_Count > 0);
    pragma Assert (Transaction_Outputs(Transaction)'Last = Outputs_Count - 1);
    pragma Assert (Transaction_Outputs(Transaction)'Length = Outputs_Count);
    pragma Assert (Serialized_Cumulative_Data_Lengths(Outputs_Count-1) = Bytes_Processed_within_Loop);
    pragma Assert (
        Outputs_Serialized_Cumulative_Data_Lengths(
           Transaction.Outputs(0..Outputs_Count-1)
        )(Outputs_Count-1)
        = Serialized_Cumulative_Data_Lengths(Outputs_Count-1)
    );
    pragma Assert (
        Outputs_Serialized_Cumulative_Data_Lengths(
           Transaction.Outputs(0..Outputs_Count-1)
        )(Outputs_Count-1)
        = Outputs_Serialized_Data_Length(Transaction.Outputs(0..Outputs_Count-1))
    );

    Data_Checkpointing.Force_Set_Data_Tag(Saved_Data_Tag, Data_Tag_State);

    pragma Assert_And_Cut (
        Status_OK
        and then
        Transaction_Outputs_Count(Transaction) = Outputs_Count
        and then
        Current_Data_Tag = Saved_Data_Tag
        and then
        Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction))
            <= Long_Natural(Outputs.Output_Model_Max_Serialized_Data_Size) * Long_Natural(Outputs_Count)
        and then
        Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction))
            = Long_Natural(Bytes_Processed_Since(Raw_Data_Status_Old))
        and then
        Transaction_Version(Transaction) = Transaction_Version(Transaction_Old)
        and then
        Transaction_Inputs_Count(Transaction) = Transaction_Inputs_Count(Transaction_Old)
        and then
        Transaction_Inputs(Transaction) = Transaction_Inputs(Transaction_Old)
    );
end Deserialize_Outputs;

procedure Deserialize_Input_Witnesses(Transaction: in out Transaction_Type)
is
    subtype Serialized_Length_Cumulative_Sums_Type is Input_Witnesses_Serialized_Cumulative_Sums_Type (Input_Index_Type);

    Bytes_Processed_at_Loop_Start      : Long_Natural with Ghost;
    Bytes_Processed_before_Loop        : Long_Natural := Long_Natural(Bytes_Processed) with Ghost;
    Bytes_Processed_within_Loop        : Long_Natural := 0 with Ghost;
    Item_Serialized_Length             : Long_Natural with Ghost;
    Serialized_Cumulative_Data_Lengths : Serialized_Length_Cumulative_Sums_Type := (others => 0) with Ghost;
    Transaction_Prev                   : Transaction_Type := Transaction with Ghost;
    Transaction_Old                    : Transaction_Type := Transaction with Ghost;

    Saved_Data_Tag : Model_Tag_Type := Current_Data_Tag;
    Inputs_Count   : Input_Array_Length_Type := Transaction_Inputs_Count(Transaction);
begin
    Transaction.Input_Witnesses_Count := 0;

    if Inputs_Count = 0 then
        return;
    end if;

    for Index in 0..Integer(Inputs_Count)-1 loop

        Bytes_Processed_at_Loop_Start := Long_Natural(Bytes_Processed);

        Input_Witnesses.Deserialize(Transaction.Input_Witnesses(Index));

        if Got_Error then
            return; -- caller will check the error and set transaction to empty
        end if;

        Transaction.Input_Witnesses_Count := Index + 1;

        pragma Assert (Transaction_Input_Witnesses(Transaction)'Length = Index + 1);
        pragma Assert (Transaction_Input_Witnesses(Transaction)'Last = Index);

        pragma Assert (
            Transaction.Input_Witnesses = Transaction_Prev.Input_Witnesses'Update(
                Index => Transaction.Input_Witnesses(Index)
            )
        );

        Item_Serialized_Length := Long_Natural(Bytes_Processed) - Bytes_Processed_at_Loop_Start;
        pragma Assert (Item_Serialized_Length > 0);

        Bytes_Processed_within_Loop := Bytes_Processed_within_Loop + Item_Serialized_Length;

        pragma Assert (Bytes_Processed_within_Loop > 0);
        pragma Assert (Item_Serialized_Length <= Long_Natural(Input_Witnesses.Input_Witness_Model_Max_Serialized_Data_Size));
        pragma Assert (
            Input_Witnesses.Input_Witness_Serialized_Data_Length(Transaction.Input_Witnesses(Index))
                = Item_Serialized_Length
        );

        Serialized_Cumulative_Data_Lengths(Index) :=
            Item_Serialized_Length + (
                                        if Index = 0
                                        then 0
                                        else Serialized_Cumulative_Data_Lengths(Index - 1)
                                     );

        if Index = 0 then
            pragma Assert (
                Serialized_Cumulative_Data_Lengths(Index)
                    = Input_Witnesses.Input_Witness_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction)(Index))
            );
            pragma Assert (
                Input_Witnesses_Serialized_Cumulative_Data_Lengths(Transaction_Input_Witnesses(Transaction))(Index)
                    = Input_Witnesses.Input_Witness_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction)(Index))
            );
            pragma Assert (
                Serialized_Cumulative_Data_Lengths(Index)
                    = Input_Witnesses_Serialized_Cumulative_Data_Lengths(Transaction.Input_Witnesses(0..Index))(Index)
            );
        else
            Input_Witnesses_Serialized_Cumulative_Data_Lengths_Eq_Lemma(
                Transaction.Input_Witnesses(0..Index), Transaction_Prev.Input_Witnesses(0..Index-1)
            );
            pragma Assert (
                for all I in 0..Index-1 =>
                   Input_Witnesses_Serialized_Cumulative_Data_Lengths(Transaction.Input_Witnesses(0..Index))(I)
                        = Input_Witnesses_Serialized_Cumulative_Data_Lengths(Transaction_Prev.Input_Witnesses(0..Index-1))(I)
            );
            pragma Assert (
                Serialized_Cumulative_Data_Lengths(Index - 1)
                    = Input_Witnesses_Serialized_Cumulative_Data_Lengths(Transaction.Input_Witnesses(0..Index))(Index - 1)
            );
            pragma Assert (
                Serialized_Cumulative_Data_Lengths(Index)
                    = Input_Witnesses.Input_Witness_Serialized_Data_Length(Transaction.Input_Witnesses(Index))
                    + Serialized_Cumulative_Data_Lengths(Index - 1)
            );
            pragma Assert (
                Input_Witnesses_Serialized_Cumulative_Data_Lengths(Transaction.Input_Witnesses(0..Index))(Index)
                    = Input_Witnesses.Input_Witness_Serialized_Data_Length(Transaction.Input_Witnesses(Index))
                        + Input_Witnesses_Serialized_Cumulative_Data_Lengths(Transaction.Input_Witnesses(0..Index))(Index - 1)
            );
            pragma Assert (
                Serialized_Cumulative_Data_Lengths(Index)
                    = Input_Witnesses_Serialized_Cumulative_Data_Lengths(Transaction.Input_Witnesses(0..Index))(Index)
            );
        end if;
        pragma Loop_Invariant (
            Serialized_Cumulative_Data_Lengths(Index)
                = Input_Witnesses_Serialized_Cumulative_Data_Lengths(Transaction.Input_Witnesses(0..Index))(Index)
        );
        pragma Loop_Invariant (
            for all I in 0..Index =>
                Serialized_Cumulative_Data_Lengths(I)
                    = Input_Witnesses_Serialized_Cumulative_Data_Lengths(Transaction.Input_Witnesses(0..Index))(I)
        );
        pragma Loop_Invariant (Status_OK);
        pragma Loop_Invariant (Bytes_Processed_within_Loop > 0);
        pragma Loop_Invariant (Transaction_Input_Witnesses(Transaction)'Length = Index + 1);
        pragma Loop_Invariant(
            Serialized_Cumulative_Data_Lengths(Index) <=
                Long_Natural(Input_Witnesses.Input_Witness_Model_Max_Serialized_Data_Size)
                    * Long_Natural(Index + 1)
        );
        pragma Loop_Invariant(
            for all I in 0..Index =>
                Serialized_Cumulative_Data_Lengths(I) <=
                    Long_Natural(Input_Witnesses.Input_Witness_Model_Max_Serialized_Data_Size)
                        * Long_Natural(I + 1)
        );
        pragma Loop_Invariant (
            if Index = 0 then
                Bytes_Processed_within_Loop = Item_Serialized_Length
            else
                Bytes_Processed_within_Loop =
                    Item_Serialized_Length + Serialized_Cumulative_Data_Lengths(Index - 1)
        );
        pragma Loop_Invariant (
            Bytes_Processed_within_Loop = Serialized_Cumulative_Data_Lengths(Index)
        );
        pragma Loop_Invariant(
            Bytes_Processed_within_Loop
                = Long_Natural(Bytes_Processed) - Bytes_Processed_before_Loop
        );
        Transaction_Prev := Transaction;
    end loop;
    pragma Assert (Bytes_Processed_within_Loop > 0);

    pragma Assert (Bytes_Processed_within_Loop = Long_Natural(Bytes_Processed) - Bytes_Processed_before_Loop);
    pragma Assert (Transaction_Input_Witnesses(Transaction)'Last = Inputs_Count - 1);
    pragma Assert (Transaction_Input_Witnesses(Transaction)'Length = Inputs_Count);
    pragma Assert (
        Input_Witnesses_Serialized_Cumulative_Data_Lengths(
           Transaction_Input_Witnesses(Transaction)
        )(Transaction_Input_Witnesses(Transaction)'Last)
        = Serialized_Cumulative_Data_Lengths(Transaction_Input_Witnesses(Transaction)'Last)
    );
    pragma Assert (
        Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction))
            = Bytes_Processed_within_Loop
    );

    Data_Checkpointing.Force_Set_Data_Tag(Saved_Data_Tag, Data_Tag_State);

    pragma Assert_And_Cut (
        Status_OK
        and then
        Transaction_Inputs_Count(Transaction) = Transaction_Inputs_Count(Transaction_Old)
        and then
        Transaction_Outputs_Count(Transaction) = Transaction_Outputs_Count(Transaction_Old)
        and then
        Transaction_Input_Witnesses(Transaction)'Length = Transaction_Inputs_Count(Transaction)
        and then
        Current_Data_Tag = Saved_Data_Tag
        and then
        Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction))
            <= Long_Natural(Input_Witnesses.Input_Witness_Model_Max_Serialized_Data_Size)
                * Long_Natural(Inputs_Count)
        and then
        Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction))
            = Long_Natural(Bytes_Processed) - Bytes_Processed_before_Loop
        and then
        Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction))
             = Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction_Old))
        and then
        Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction))
             = Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction_Old))
    );
end Deserialize_Input_Witnesses;

procedure Deserialize(Transaction: out Transaction_Type)
is
    use Data_Checkpoints;

    Inputs_Count  : Compact_Size_Type;
    Outputs_Count : Compact_Size_Type;
    U32_Value     : Unsigned_Ranged_Int32;
    Flag_Byte     : Byte_Type;
    Has_Witnesses : Boolean := False;

    Raw_Data_Status_Old     : Raw_Data.Status_Type := Raw_Data_Status with Ghost;
    Transaction_Prev        : Transaction_Type with Ghost;
    Transaction_Inputs_Prev : Transaction_Inputs_Bounded_Array_Type with Ghost;
begin
        -- Before we get 'Initialized attribute for arrays/records, we have to initialize
        -- beforehand, as we might not fill all the items, and if we fill the itmes in the loop,
        -- flow analysis currently cannot detect that all the items were actually initialized,
        -- unfortunately. We will reset Transaction to empty on all errors, though, anyway.
        Transaction := Empty_Transaction;

        Data_Checkpoint(Tag_Version);

        Data_Readers.Read_Unsigned_32(U32_Value);

        if Got_Error then
            Transaction := Empty_Transaction;
            return;
        end if;

        if (
            U32_Value not in Unsigned_Ranged_Int32(Version_Type'First)..Unsigned_Ranged_Int32(Version_Type'Last)
        ) then
            Transaction := Empty_Transaction;
            Register_Structural_Error(
                "Transaction Version " & Unsigned_Ranged_Int32'Image(U32_Value) & " is not supported"
            );
            return;
        end if;

        Transaction.Version := Version_Type(U32_Value);

        Data_Checkpoint(Tag_Witness_Marker);

        pragma Assert_And_Cut(
             Status_OK
             and then
             Current_Data_Tag = Tag_Witness_Marker
             and then
             Data_Checkpoints.Partial_State_Consistent(Bytes_Processed_Since(Raw_Data_Status_Old), Tag_Version)
             and then (Bytes_Processed = Bytes_Processed_With(Raw_Data_Status_Old) + 4)
             and then Data_Checkpoints.Checkpoint_States.Bytes_Processed_Prev(Data_Checkpoints.State) = Bytes_Processed
        );

        Data_Readers.Read_Compact_Size(Inputs_Count);

        if Got_Error then
            Transaction := Empty_Transaction;
            return;
        end if;

        if Inputs_Count = 0 then
            Data_Readers.Read_Byte(Flag_Byte);
            if Got_Error then
                Transaction := Empty_Transaction;
                return;
            end if;
            if Flag_Byte /= 1 then
                Transaction := Empty_Transaction;
                Register_Structural_Error(
                    "Unexpected flag byte (" & Byte_Type'Image(Flag_Byte) & ") after segwit marker, "
                    & "expected 1"
                );
                return;
            end if;
            Has_Witnesses := True;

            Data_Checkpoint(Tag_Inputs_Count);

            pragma Assert (Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 2));

            Data_Readers.Read_Compact_Size(Inputs_Count);
            if Got_Error then
                Transaction := Empty_Transaction;
                return;
            end if;

            if Inputs_Count = 0 then
                Register_Structural_Error("Transaction has no inputs");
                return;
            end if;
            pragma Assert (
                 Data_Checkpoints.Partial_State_Consistent(
                     Bytes_Processed_Since(Raw_Data_Status_Old), Tag_Witness_Marker)
            );
        else
            Data_Checkpoint_Skip(Tag_Witness_Marker);
            pragma Assert (Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 0));
            pragma Assert (
                 Data_Checkpoints.Partial_State_Consistent(
                     Bytes_Processed_Since(Raw_Data_Status_Old), Tag_Witness_Marker)
            );
        end if;

        pragma Assert_And_Cut(
             Status_OK
             and then
             Inputs_Count > 0
             and then
             Current_Data_Tag = Tag_Inputs_Count
             and then
             Data_Checkpoints.Partial_State_Consistent(
                 Bytes_Processed_Since(Raw_Data_Status_Old), Tag_Witness_Marker)
             and then
             (
                if Has_Witnesses
                then Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 2)
                else Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 0)
             )
             and then (Bytes_Processed = Bytes_Processed_With(Raw_Data_Status_Old) + 4 + (if Has_Witnesses then 2 else 0) + Size_of_Compact_Size(Inputs_Count))
             and then Data_Checkpoints.Checkpoint_States.Bytes_Processed_Prev(Data_Checkpoints.State) + Size_of_Compact_Size(Inputs_Count) = Bytes_Processed
        );

        if Inputs_Count > Max_Inputs then
            Transaction := Empty_Transaction;
            Register_Structural_Error(
                "Number of inputs (" & Compact_Size_Type'Image(Inputs_Count) & ") is too large, "
                & "maximum " & Positive'Image(Max_Inputs) & " inputs in a transaction can be accepted"
            );
            return;
        end if;

        Data_Checkpoint(Tag_Inputs);

        pragma Assert_And_Cut(
             Status_OK
             and then
             (Inputs_Count > 0 and Inputs_Count <= Max_Inputs)
             and then
             Current_Data_Tag = Tag_Inputs
             and then
             Data_Checkpoints.Partial_State_Consistent(
                 Bytes_Processed_Since(Raw_Data_Status_Old), Tag_Inputs_Count)
             and then
             (
                if Has_Witnesses
                then Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 2)
                else Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 0)
             )
             and then
             Data_Checkpoints.Element_Size_Match(Tag_Inputs_Count,
                    Long_Natural(Size_of_Compact_Size(Inputs_Count)))
             and then (Bytes_Processed = Bytes_Processed_With(Raw_Data_Status_Old) + 4 + (if Has_Witnesses then 2 else 0) + Size_of_Compact_Size(Inputs_Count))
             and then Data_Checkpoints.Checkpoint_States.Bytes_Processed_Prev(Data_Checkpoints.State) = Bytes_Processed
        );

        Deserialize_Inputs(Transaction, Inputs_Count);

        if Got_Error then
            Transaction := Empty_Transaction;
            return;
        end if;

        pragma Assert (Transaction.Inputs_Count = Inputs_Count);

        Data_Checkpoint(Tag_Outputs_Count);

        -- unrolled Partial_State_Consistent
--        pragma Assert (for all T in Tag_Version..Tag_Inputs => Data_Checkpoints.Checkpoint_States.Tag_Seen(T, Data_Checkpoints.State));
--        pragma Assert (
--            for all T in Model_Range_Type'Succ(Tag_Inputs)..Tag_Lock_Time =>
--            not Data_Checkpoints.Checkpoint_States.Tag_Seen(T, Data_Checkpoints.State)
--            and then Data_Checkpoints.Checkpoint_States.Element_Size(T, Data_Checkpoints.State) = 0
--            and then Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(T, Data_Checkpoints.State) = 0
--        );
--        pragma Assert (
--            Transaction_Data_Model(Tag_Inputs).From <= Data_Checkpoints.Checkpoint_States.Element_Size(Tag_Inputs, Data_Checkpoints.State)
--        );
--        pragma Assert (
--            Data_Checkpoints.Checkpoint_States.Element_Size(Tag_Inputs, Data_Checkpoints.State) <= Transaction_Data_Model(Tag_Inputs).To
--        );
--        pragma Assert (
--            for all T in Tag_Version..Tag_Inputs =>
--                (Transaction_Data_Model(T).From <= Data_Checkpoints.Checkpoint_States.Element_Size(T, Data_Checkpoints.State) and Data_Checkpoints.Checkpoint_States.Element_Size(T, Data_Checkpoints.State) <= Transaction_Data_Model(T).To)
--        );

        pragma Assert_And_Cut(
             Status_OK
             and then
             Transaction_Inputs_Count(Transaction) > 0
             and then
             Current_Data_Tag = Tag_Outputs_Count
             and then
             Data_Checkpoints.Partial_State_Consistent(
                 Bytes_Processed_Since(Raw_Data_Status_Old), Tag_Inputs)
             and then
             (
                if Has_Witnesses
                then Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 2)
                else Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 0)
             )
             and then
             Data_Checkpoints.Element_Size_Match(
                     Tag_Inputs_Count, Long_Natural(Size_of_Compact_Size(Transaction_Inputs_Count(Transaction))))
             and then
             Data_Checkpoints.Element_Size_Match(
                     Tag_Inputs, Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction)))
             and then (Bytes_Processed = Bytes_Processed_With(Raw_Data_Status_Old) + 4 + (if Has_Witnesses then 2 else 0) + Size_of_Compact_Size(Size_of_Compact_Size(Transaction_Inputs_Count(Transaction))) + Natural(Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction))))
             and then Data_Checkpoints.Checkpoint_States.Bytes_Processed_Prev(Data_Checkpoints.State) = Bytes_Processed
        );

        Data_Readers.Read_Compact_Size(Outputs_Count);

        if Got_Error then
            Transaction := Empty_Transaction;
            return;
        end if;

        if Outputs_Count > Max_Outputs then
            Transaction := Empty_Transaction;
            Register_Structural_Error(
                "Number of outputs (" & Compact_Size_Type'Image(Outputs_Count) & ") is too large, "
                & "maximum " & Positive'Image(Max_Outputs) & " outputs in a transaction can be accepted"
            );
            return;
        end if;

        Data_Checkpoint(Tag_Outputs);

        pragma Assert_And_Cut(
             Status_OK
             and then
             Transaction_Inputs_Count(Transaction) > 0
             and then
             Outputs_Count <= Max_Outputs
             and then
             Current_Data_Tag = Tag_Outputs
             and then
             Data_Checkpoints.Partial_State_Consistent(
                 Bytes_Processed_Since(Raw_Data_Status_Old), Tag_Outputs_Count)
             and then
             (
                if Has_Witnesses
                then Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 2)
                else Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 0)
             )
             and then
             Data_Checkpoints.Element_Size_Match(
                     Tag_Inputs_Count, Long_Natural(Size_of_Compact_Size(Transaction_Inputs_Count(Transaction))))
             and then
             Data_Checkpoints.Element_Size_Match(
                     Tag_Inputs, Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction)))
             and then
             Data_Checkpoints.Element_Size_Match(
                    Tag_Outputs_Count, Long_Natural(Size_of_Compact_Size(Outputs_Count)))
             and then (Bytes_Processed = Bytes_Processed_With(Raw_Data_Status_Old) + 4 + (if Has_Witnesses then 2 else 0) + Size_of_Compact_Size(Size_of_Compact_Size(Transaction_Inputs_Count(Transaction))) + Natural(Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction))) + Size_of_Compact_Size(Outputs_Count))
             and then Data_Checkpoints.Checkpoint_States.Bytes_Processed_Prev(Data_Checkpoints.State) = Bytes_Processed
        );

        -- After Deserialize_Outputs, for some reason the prover loses the knowledge
        -- about Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction))
        -- and Element_Size_Match on Tag_Inputs, even though Deserialize_Outputs has a postcondition
        -- that states Transaction_Inputs(Transaction) = Transaction_Inputs(Transaction'Old)
        -- So we need to transfer that knowledge manually.
        Transaction_Inputs_Prev := Transaction.Inputs;
        pragma Assert (
            Transaction_Inputs_Prev(0..Transaction_Inputs_Count(Transaction)-1)
                = Transaction_Inputs(Transaction)
        );
        pragma Assert (
            Data_Checkpoints.Element_Size_Match(
                Tag_Inputs,
                Inputs_Serialized_Data_Length(
                    Transaction_Inputs_Prev(0..Transaction_Inputs_Count(Transaction)-1)))
        );

        Deserialize_Outputs(Transaction, Outputs_Count);

        if Got_Error then
            Transaction := Empty_Transaction;
            return;
        end if;

        -- Make sure the needed facts still exist after Deserialize_Outputs
        pragma Assert (
            Data_Checkpoints.Element_Size_Match(
                Tag_Inputs,
                Inputs_Serialized_Data_Length(
                    Transaction_Inputs_Prev(0..Transaction_Inputs_Count(Transaction)-1)))
        );
        pragma Assert (
            Transaction_Inputs_Prev(0..Transaction_Inputs_Count(Transaction)-1)
                = Transaction_Inputs(Transaction)
        );
        Inputs_Serialized_Cumulative_Data_Lengths_Eq_Lemma(
            Transaction_Inputs_Prev(0..Transaction_Inputs_Count(Transaction)-1),
            Transaction_Inputs(Transaction)
        );
        pragma Assert (
             Data_Checkpoints.Element_Size_Match(
                 Tag_Inputs, Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction)))
        );

        pragma Assert (Transaction.Outputs_Count = Outputs_Count);

        Data_Checkpoint(Tag_Input_Witnesses);

        pragma Assert_And_Cut(
             Status_OK
             and then
             Transaction_Inputs_Count(Transaction) > 0
             and then
             Current_Data_Tag = Tag_Input_Witnesses
             and then
             Data_Checkpoints.Partial_State_Consistent(
                 Bytes_Processed_Since(Raw_Data_Status_Old), Tag_Outputs)
             and then
             (
                if Has_Witnesses
                then Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 2)
                else Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 0)
             )
             and then
             Data_Checkpoints.Element_Size_Match(
                 Tag_Inputs_Count, Long_Natural(Size_of_Compact_Size(Transaction_Inputs_Count(Transaction))))
             and then
             Data_Checkpoints.Element_Size_Match(
                 Tag_Inputs, Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction)))
             and then
             Data_Checkpoints.Element_Size_Match(
                Tag_Outputs_Count, Long_Natural(Size_of_Compact_Size(Transaction_Outputs_Count(Transaction))))
             and then
             Data_Checkpoints.Element_Size_Match(
                 Tag_Outputs, Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction)))
             and then (Bytes_Processed = Bytes_Processed_With(Raw_Data_Status_Old) + 4 + (if Has_Witnesses then 2 else 0) + Size_of_Compact_Size(Size_of_Compact_Size(Transaction_Inputs_Count(Transaction))) + Natural(Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction))) + Size_of_Compact_Size(Transaction_Outputs_Count(Transaction)) + Natural(Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction))))
             and then Data_Checkpoints.Checkpoint_States.Bytes_Processed_Prev(Data_Checkpoints.State) = Bytes_Processed
        );

        if Has_Witnesses then
            Deserialize_Input_Witnesses(Transaction);

            if Got_Error then
                Transaction := Empty_Transaction;
                return;
            end if;

            if not Transaction_Has_Witness(Transaction) then
                Transaction := Empty_Transaction;
                Register_Structural_Error(
                    "Transaction has flag that indicates that it has witness data, but all witnesses are empty"
                );
                return;
            end if;
            pragma Assert (Transaction_Has_Witness(Transaction));
            pragma Assert (Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction)) > 0);
        else
            Transaction.Input_Witnesses_Count := 0;
            pragma Assert (not Transaction_Has_Witness(Transaction));
            pragma Assert (Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction)) = 0);
        end if;

        pragma Assert (Has_Witnesses = Transaction_Has_Witness(Transaction));

        Data_Checkpoint(Tag_Lock_Time);

        -- help the prover to 'remember' important facts
        pragma Assert (
             Data_Checkpoints.Element_Size_Match(
                 Tag_Outputs, Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction)))
        );
        pragma Assert (
            Data_Checkpoints.Checkpoint_States.Element_Size(Tag_Input_Witnesses, Data_Checkpoints.State)
                = Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction))
        );
        pragma Assert (
            Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction))
                <= Long_Natural(Input_Witnesses.Input_Witness_Model_Max_Serialized_Data_Size) * Long_Natural(Max_Inputs)
        );

        pragma Assert_And_Cut(
             Status_OK
             and then
             Transaction_Inputs_Count(Transaction) > 0
             and then
             Current_Data_Tag = Tag_Lock_Time
             and then
             Data_Checkpoints.Partial_State_Consistent(
                 Bytes_Processed_Since(Raw_Data_Status_Old), Tag_Input_Witnesses)
             and then
             (
                if Transaction_Has_Witness(Transaction)
                then Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 2)
                else Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 0)
             )
             and then
             (
                if Transaction_Has_Witness(Transaction)
                then Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction)) > 0
                else Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction)) = 0
             )
             and then
             Data_Checkpoints.Element_Size_Match(
                 Tag_Inputs_Count, Long_Natural(Size_of_Compact_Size(Transaction_Inputs_Count(Transaction))))
             and then
             Data_Checkpoints.Element_Size_Match(
                 Tag_Inputs, Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction)))
             and then
             Data_Checkpoints.Element_Size_Match(
                 Tag_Outputs_Count, Long_Natural(Size_of_Compact_Size(Transaction_Outputs_Count(Transaction))))
             and then
             Data_Checkpoints.Element_Size_Match(
                 Tag_Outputs, Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction)))
             and then
             Data_Checkpoints.Element_Size_Match(
                 Tag_Input_Witnesses,
                 Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction)))
             and then (Bytes_Processed = Bytes_Processed_With(Raw_Data_Status_Old) + 4 + (if Transaction_Has_Witness(Transaction) then 2 else 0) + Size_of_Compact_Size(Size_of_Compact_Size(Transaction_Inputs_Count(Transaction))) + Natural(Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction))) + Size_of_Compact_Size(Transaction_Outputs_Count(Transaction)) + Natural(Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction))) + Natural(Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction))))
             and then Data_Checkpoints.Checkpoint_States.Bytes_Processed_Prev(Data_Checkpoints.State) = Bytes_Processed
        );

        Data_Readers.Read_Unsigned_32(U32_Value);

        if Got_Error then
            Transaction := Empty_Transaction;
            return;
        end if;

        -- save transaction struct so that we can state that only Lock_Time was modified in the code below
        Transaction_Prev := Transaction;

        if (
            U32_Value in Unsigned_Ranged_Int32(Lock_Time_Unlocked_Type'First)
                            .. Unsigned_Ranged_Int32(Lock_Time_Unlocked_Type'Last)
        ) then
            Transaction.Lock_Time := (Kind => Lock_Time_Unlocked);
        elsif (
            U32_Value in Unsigned_Ranged_Int32(Lock_Time_Block_Number_Type'First)
                            .. Unsigned_Ranged_Int32(Lock_Time_Block_Number_Type'Last)
        ) then
            Transaction.Lock_Time := (
                Kind => Lock_Time_Block_Number,
                Block_Number => Lock_Time_Block_Number_Type(U32_Value)
            );
        elsif (
            U32_Value in Unsigned_Ranged_Int32(Lock_Time_Timestamp_Type'First)
                            .. Unsigned_Ranged_Int32(Lock_Time_Timestamp_Type'Last)
        ) then
            Transaction.Lock_Time := (
                Kind => Lock_Time_Timestamp,
                Timestamp => Lock_Time_Timestamp_Type(U32_Value)
            );
        else
            raise Program_Error; -- unreachable
        end if;

        -- show that only Lock_Time was changed
        pragma Assert (
            Transaction = Transaction_Prev'Update(
                Lock_Time => Transaction.Lock_Time
            )
        );
        -- and that input witnesses did not change
        pragma Assert (
            Transaction_Input_Witnesses(Transaction) = Transaction_Input_Witnesses(Transaction_Prev)
        );

--        -- 'Remind' the prover that this fact is important, it tends to lose it
--        pragma Assert (
--            if Transaction_Has_Witness(Transaction)
--            then Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 2)
--            else Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 0)
--        );

        Data_Checkpoint_Final;

        -- Had to thread all conditions through Assert_And_Cut to cut on the prover state,
        -- otherwise cannot postcondition even with plenty of resources and time
        -- It would be nice to be able to build Assert_And_Cut condition incrementally where you
        -- could use previous Assert_And_Cut conditions without restating them again
        pragma Assert_And_Cut(
             Status_OK
             and then
             Transaction_Inputs_Count(Transaction) > 0
             and then
             Data_Checkpoints.Final_State_Consistent(Bytes_Processed_Since(Raw_Data_Status_Old))
             and then
             (
                if Transaction_Has_Witness(Transaction)
                then Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 2)
                else Data_Checkpoints.Element_Size_Match(Tag_Witness_Marker, 0)
             )
             and then
             (
                if Transaction_Has_Witness(Transaction)
                then Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction)) > 0
                else Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction)) = 0
             )
             and then
             Data_Checkpoints.Element_Size_Match(
                     Tag_Inputs_Count, Long_Natural(Size_of_Compact_Size(Transaction_Inputs_Count(Transaction))))
             and then
             Data_Checkpoints.Element_Size_Match(
                     Tag_Inputs, Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction)))
             and then
             Data_Checkpoints.Element_Size_Match(
                    Tag_Outputs_Count, Long_Natural(Size_of_Compact_Size(Transaction_Outputs_Count(Transaction))))
             and then
             Data_Checkpoints.Element_Size_Match(
                     Tag_Outputs, Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction)))
             and then
             Data_Checkpoints.Element_Size_Match(
                 Tag_Input_Witnesses,
                 Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction)))
             and then (Bytes_Processed = Bytes_Processed_With(Raw_Data_Status_Old) + 4 + (if Transaction_Has_Witness(Transaction) then 2 else 0) + Size_of_Compact_Size(Size_of_Compact_Size(Transaction_Inputs_Count(Transaction))) + Natural(Inputs_Serialized_Data_Length(Transaction_Inputs(Transaction))) + Size_of_Compact_Size(Transaction_Outputs_Count(Transaction)) + Natural(Outputs_Serialized_Data_Length(Transaction_Outputs(Transaction))) + Natural(Input_Witnesses_Serialized_Data_Length(Transaction_Input_Witnesses(Transaction))) + 4)
             and then Data_Checkpoints.Checkpoint_States.Bytes_Processed_Prev(Data_Checkpoints.State) = Bytes_Processed
        );
end;

end Bitcoin_Like.Transactions;
