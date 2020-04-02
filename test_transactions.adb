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

with Ada.Text_IO;

with Bitcoin;

with Bitcoin.Transactions;
with Bitcoin_Like.Data_Accessors;
with Bitcoin_Like.Utils; use Bitcoin_Like.Utils;
with Stdio;

package body Test_Transactions is

    package body Byte_Readers is
        procedure Read_Byte(Byte: out Byte_Type) is
            Two_Chars : String(1..2) := (others => Character'Val(0));
            Has_Read  : Boolean;
        begin
            if Bytes_Processed = Natural'Last then
                Byte := 0;
                Register_Structural_Error("Too much data has been read, transaction cannot be this long");
                return;
            end if;
            Stdio.Char_Readers.Read_Char(Two_Chars(1), Has_Read);
            if not Has_Read then
                Byte := 0;
                Register_Raw_Data_Error(Error_End_of_Stream);
                return;
            end if;
            Stdio.Char_Readers.Read_Char(Two_Chars(2), Has_Read);
            if not Has_Read then
                Byte := 0;
                Register_Raw_Data_Error(Error_Invalid_Data_Encoding);
                return;
            end if;
            if not Is_Hex_Digit(Two_Chars(1)) or not Is_Hex_Digit(Two_Chars(2)) then
                Byte := 0;
                Register_Raw_Data_Error(Error_Invalid_Data_Encoding);
            else
                Byte := Hex_To_Byte(Two_Chars);
                Ghost_Data_Tape(Bytes_Processed) := Byte;
                Increase_Bytes_Processed(1);
            end if;
        end;
    end Byte_Readers;

    package body Byte_Writers is
        procedure Write_Byte(Byte: in Byte_Type)
        is begin
            if Bytes_Processed = Natural'Last then
                Register_Structural_Error("Too much data has been written, transaction cannot be this long");
                return;
            end if;
            if Bytes_Processed <= Write_Limit then
                Increase_Bytes_Processed(1);
            else
                Register_Raw_Data_Error(Error_Storage_Full);
            end if;
        end Write_Byte;
    end Byte_Writers;

    pragma Warnings (Off, "unused variable ""Byte"", in instantiation",
                     Reason => "Test writer does not use Byte");
    package Test_Byte_Writers is new Byte_Writers (Write_Limit => 1);
    pragma Warnings (On, "unused variable ""Byte"", in instantiation");

    package Data_Readers is new Bitcoin_Like.Data_Accessors.Readers(
         Status_Control => Read_Status,
         Read_Byte_Procedure => Byte_Readers.Read_Byte
    );
    package Data_Writers is new Bitcoin_Like.Data_Accessors.Writers(
         Status_Control => Write_Status,
         Write_Byte_Procedure => Test_Byte_Writers.Write_Byte
    );

    package Bitcoin_Transactions is new Bitcoin.Transactions(
        Data_Readers => Data_Readers,
        Data_Writers => Data_Writers
    );

    use Bitcoin_Transactions;
    use Transaction_Model_Types;
    use Transactions;

    procedure Put_Line_and_Return_Length(The_Line: in String; Result_Length: out Natural)
    is begin
        Ada.Text_IO.Put_Line(The_Line);
        Result_Length := The_Line'Length;
    end;

    procedure Put_Line_of_Chars(Char: in Character; Num_Chars: in Natural)
    is begin
        for Index in 1..Num_Chars loop
            Ada.Text_IO.Put(Char);
            pragma Loop_Invariant(Index <= Num_Chars); -- otherwise prover cannot unroll loop
        end loop;
        Ada.Text_IO.Put(ASCII.LF);
    end;

    procedure Test_Transaction is
        Transaction : Transaction_Type;
        Lock_Time   : Lock_Time_Variant;
        String_Buffer : String(1..8192) := (others => ' ');
        String_Buffer_Max_Size: constant Natural := 8192;
        String_Buffer_Limit : Natural range 1..String_Buffer_Max_Size;
        Input: Inputs.Input_Type;
        Output: Outputs.Output_Type;
        Input_Witness: Input_Witnesses.Input_Witness_Type;
        Stack_Item: Input_Witnesses.Input_Witness_Stack_Item_Type;
        Prev_Line_Length: Natural;
    begin
        Transactions.Deserialize(Transaction);
        if Read_Status.Got_Error then
            Ada.Text_IO.Put_Line("Transaction is invalid after deserialize");
            if Read_Status.Got_Raw_Data_Error then
                Ada.Text_IO.Put_Line(
                    "Data read error " & Reader_Error_Code_Type'Image(Read_Status.Raw_Data_Error_Code)
                );
            elsif Read_Status.Got_Structural_Error then
                pragma Assert (Read_Status.Structural_Error_Message'Length > 0);
                Ada.Text_IO.Put_Line("Deserialization error ");
                Ada.Text_IO.Put_Line(Read_Status.Structural_Error_Message);
            else
                raise Program_Error;
            end if;

            Ada.Text_IO.Put_Line("consumed bytes: " & Positive'Image(Read_Status.Bytes_Processed));
            Ada.Text_IO.Put_Line(
                "last processed data tag: " & Tag_Type'Image(Read_Status.Current_Data_Tag)
            );
        else
            Ada.Text_IO.Put_Line("Deserialized successfully");
            Ada.Text_IO.Put_Line("");
            Put_Line_and_Return_Length(
                "Transaction version: " & Version_Type'Image(Transaction_Version(Transaction)), Prev_Line_Length);
            Put_Line_of_Chars('-', Prev_Line_Length);
            Ada.Text_IO.Put_Line("");
            Put_Line_and_Return_Length(
                "Number of inputs: " & Natural'Image(Transaction_Inputs_Count(Transaction)), Prev_Line_Length);
            Put_Line_of_Chars('-', Prev_Line_Length);
            Ada.Text_IO.Put_Line("");
            for Input_Index in Transaction_Inputs(Transaction)'Range loop
                pragma Loop_Invariant(Input_Index <= Input_Array_Length_Type'Last);

                -- Set line length, otherwise Set_Col's precondition will fail
                -- We need to do this inside the loop, because this property is lost on next iteration
                -- I guess we can add a loop invariant instead, but this is simpler.
                Ada.Text_IO.Set_Line_Length(0);

                Input := Transaction_Inputs(Transaction)(Input_Index);
                Ada.Text_IO.Put_Line("Input " & Input_Index_Type'Image(Input_Index));
                String_Buffer_Limit := Input.Previous_Output_Point.Txid'Length * 2;
                Byte_Array_To_Hex_Reverse(
                    Byte_Array_Type(Input.Previous_Output_Point.Txid), String_Buffer(1..String_Buffer_Limit));
                Ada.Text_IO.Set_Col(4);
                Ada.Text_IO.Put_Line(
                    "Prevout: " & String_Buffer(1..String_Buffer_Limit) & ":"
                    & Previous_Output_Index_Type'Image(Input.Previous_Output_Point.Index));
                Ada.Text_IO.Set_Col(4);
                if Signature_Scripts.Script_Length(Input.Signature_Script) > 0 then
                    String_Buffer_Limit := Signature_Scripts.Script_Length(Input.Signature_Script) * 2;
                    Byte_Array_To_Hex(
                        Byte_Array_Type(Signature_Scripts.Script_Data(Input.Signature_Script)),
                        String_Buffer(1..String_Buffer_Limit));
                    Ada.Text_IO.Put_Line("Signature script: " & String_Buffer(1..String_Buffer_Limit));
                else
                    Ada.Text_IO.Put_Line("Signature script: empty");
                end if;
                if Transaction_Has_Witness(Transaction) then
                    Ada.Text_IO.Set_Col(4);
                    Input_Witness := Transaction_Input_Witness(Transaction, Input_Index);
                    if Input_Witnesses.Is_Input_Witness_Empty(Input_Witness) then
                        Ada.Text_IO.Put_Line("Witness is empty");
                    else
                        String_Buffer_Limit := P2WSH_Scripts.Script_Length(Input_Witness.Script) * 2;
                        Byte_Array_To_Hex(
                            Byte_Array_Type(P2WSH_Scripts.Script_Data(Input_Witness.Script)),
                            String_Buffer(1..String_Buffer_Limit));
                        Ada.Text_IO.Put_Line("Witness script: " & String_Buffer(1..String_Buffer_Limit));
                        Ada.Text_IO.Set_Col(4);
                        Ada.Text_IO.Put_Line("Witness data: ");
                        Ada.Text_IO.Set_Col(8);
                        for Stack_Item_Index in Input_Witnesses.Input_Witness_Stack_Items(Input_Witness.Stack)'Range loop
                            pragma Loop_Invariant(Stack_Item_Index <= Input_Witnesses.Input_Witness_Stack_Length_Type'Last);
                            pragma Assert (Input_Witnesses.Input_Witness_Data_Blob_Length_Type'Last / 2 < String_Buffer_Max_Size);
                            Stack_Item := Input_Witnesses.Input_Witness_Stack_Items(Input_Witness.Stack)(Stack_Item_Index);
                            String_Buffer_Limit := Input_Witnesses.Input_Witness_Stack_Item_Size(Stack_Item) * 2;
                            Byte_Array_To_Hex(
                                Byte_Array_Type(Input_Witnesses.Input_Witness_Stack_Item_Data(Stack_Item)),
                                String_Buffer(1..String_Buffer_Limit));
                            Ada.Text_IO.Put_Line(String_Buffer(1..String_Buffer_Limit));
                        end loop;
                    end if;
                end if;
                Ada.Text_IO.Put_Line("");
            end loop;
            Put_Line_and_Return_Length("Number of outputs: " & Natural'Image(Transaction_Outputs_Count(Transaction)), Prev_Line_Length);
            Put_Line_of_Chars('-', Prev_Line_Length);
            Ada.Text_IO.Put_Line("");
            for Output_Index in Transaction_Outputs(Transaction)'Range loop
                pragma Loop_Invariant(Output_Index <= Output_Array_Length_Type'Last);
                Ada.Text_IO.Set_Line_Length(0);
                Output := Transaction_Outputs(Transaction)(Output_Index);
                Ada.Text_IO.Put_Line("Output " & Output_Index_Type'Image(Output_Index));
                Ada.Text_IO.Set_Col(4);
                Ada.Text_IO.Put_Line("Value: " & Bitcoin.Satoshi_Type'Image(Output.Value));
                String_Buffer_Limit := Pubkey_Scripts.Script_Length(Output.Pubkey_Script) * 2;
                Byte_Array_To_Hex(
                    Byte_Array_Type(Pubkey_Scripts.Script_Data(Output.Pubkey_Script)),
                    String_Buffer(1..String_Buffer_Limit));
                Ada.Text_IO.Set_Col(4);
                Ada.Text_IO.Put_Line(
                    "Pubkey script: "
                    & String_Buffer(1..String_Buffer_Limit));
                Ada.Text_IO.Put_Line("");
            end loop;
            Lock_Time := Transaction_Lock_Time(Transaction);
            case Lock_Time.Kind is
                when Lock_Time_Unlocked => Put_Line_and_Return_Length("Not timelocked", Prev_Line_Length);
                when Lock_Time_Block_Number =>
                    Put_Line_and_Return_Length(
                        "Timelocked by block " & Lock_Time_Block_Number_Type'Image(Lock_Time.Block_Number),
                        Prev_Line_Length);
                when Lock_Time_Timestamp =>
                    Put_Line_and_Return_Length(
                        "Timelocked by timestamp" & Lock_Time_Timestamp_Type'Image(Lock_Time.Timestamp),
                        Prev_Line_Length);
            end case;
            Put_Line_of_Chars('-', Prev_Line_Length);
        end if;
    end Test_Transaction;

end Test_Transactions;
