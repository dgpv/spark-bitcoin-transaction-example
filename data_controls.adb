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

package body Data_Controls is

package body Raw_Data_Status_Control is

    procedure Increase_Bytes_Processed(Processed_Count: in Positive; Status: in out Status_Type)
    is begin
        Status.Bytes_Processed := Status.Bytes_Processed + Processed_Count;
    end Increase_Bytes_Processed;

    procedure Register_Error(Code: in Error_Code_Type; Status: in out Status_Type)
    is begin
        Status.Error_Code := Code;
    end Register_Error;

end Raw_Data_Status_Control;

package body Structural_Status_Control is
    procedure Register_Error(Message: String; Status: in out Status_Type)
    is begin
        Status.Error_Message := Error_Strings.To_Bounded_String(Message);
    end Register_Error;
end Structural_Status_Control;

package body Combined_Status_Control is
    package body Data_Checkpointing is
        procedure Register_Data_Tag(Tag: in Tag_Type; State: in out State_Type)
        is begin
            State.Current_Tag := Tag;
        end Register_Data_Tag;

        procedure Force_Set_Data_Tag(Tag: in Tag_Type; State: in out State_Type)
        is begin
            State.Current_Tag := Tag;
        end Force_Set_Data_Tag;
    end Data_Checkpointing;

    procedure Increase_Bytes_Processed(Processed_Count: in Positive) is
    begin
        Raw_Data.Increase_Bytes_Processed(Processed_Count, Raw_Data_Status);
    end Increase_Bytes_Processed;

    procedure Register_Raw_Data_Error(Error_Code: in Raw_Data.Error_Code_Type) is
    begin
        Raw_Data.Register_Error(Error_Code, Raw_Data_Status);
    end Register_Raw_Data_Error;

    procedure Register_Structural_Error(Message: String) is
    begin
        Structural.Register_Error(Message, Structural_Status);
    end Register_Structural_Error;

    procedure Register_Data_Tag(Tag: in Tag_Type)
    is begin
        Data_Checkpointing.Register_Data_Tag(Tag, Data_Tag_State);
    end Register_Data_Tag;
end Combined_Status_Control;

package body Data_Checkpoints is
    package body Checkpoint_States is
        procedure Register_Element(
            Tag   : in     Model_Range_Type;
            State : in out Ghost_State_Type
        ) is begin
            State.Checkpoints(Tag) := True;
            State.Element_Sizes(Tag) := Long_Natural(Bytes_Processed - State.Bytes_Processed_Prev);
            State.Ghost_Tape_Positions(Tag) := State.Bytes_Processed_Prev;
            State.Bytes_Processed_Prev := Bytes_Processed;
        end Register_Element;

        procedure Zap_Element(
            Tag   : in     Model_Range_Type;
            State : in out Ghost_State_Type
        ) is begin
            State.Checkpoints(Tag) := True;
            State.Element_Sizes(Tag) := 0;
            State.Ghost_Tape_Positions(Tag) := State.Bytes_Processed_Prev;
        end Zap_Element;

        procedure Init_State(State: out Ghost_State_Type)
        is begin
            State := (
                Bytes_Processed_Prev => Bytes_Processed,
                Bytes_Processed_at_Start => Bytes_Processed,
                others => <>
            );
        end Init_State;
    end Checkpoint_States;

    procedure Data_Checkpoint(Tag: in Model_Range_Type) is
        Prev_Tag: Model_Range_Type;
    begin
        if Tag = Model_Range_Type'First then
            Init_State(State);
        else
            Prev_Tag := Model_Range_Type'Pred(Tag);
            Register_Element(Prev_Tag, State);
        end if;
        Register_Data_Tag(Tag);
    end Data_Checkpoint;

    procedure Data_Checkpoint_Skip(Tag: in Model_Range_Type)
    is begin
        if Tag = Model_Range_Type'First then
            Init_State(State);
        end if;
        Zap_Element(Tag, State);
        Register_Data_Tag(Model_Range_Type'Succ(Tag));
    end Data_Checkpoint_Skip;

    procedure Data_Checkpoint_Final is
    begin
        Register_Element(Model_Range_Type'Last, State);
    end Data_Checkpoint_Final;
end Data_Checkpoints;

end Data_Controls;
