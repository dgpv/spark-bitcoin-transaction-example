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
with Bitcoin_Like.Transaction_Model; use Bitcoin_Like.Transaction_Model;
with Bitcoin_Like.Utils; use Bitcoin_Like.Utils;
with Bitcoin_Like.Scripts;

with Data_Controls;
use  Data_Controls;


package Test_Transactions is

type Byte_Array is array (Positive range <>) of Byte_Type;
type Reader_Error_Code_Type is (No_Error, Error_Invalid_Data_Encoding, Error_End_of_Stream);
type Writer_Error_Code_Type is (No_Error, Error_Storage_Full);

package Read_Data_Stratus_Control is new Raw_Data_Status_Control(
     Error_Code_Type => Reader_Error_Code_Type
);
package Read_Structural_Stratus_Control is new Structural_Status_Control;

package Write_Data_Status_Control is new Raw_Data_Status_Control(
     Error_Code_Type => Writer_Error_Code_Type
);
package Write_Structural_Stratus_Control is new Structural_Status_Control;

package Read_Status is new Combined_Status_Control(
    Raw_Data => Read_Data_Stratus_Control,
    Structural => Read_Structural_Stratus_Control,
    Model_Types => Transaction_Model_Types
);

package Write_Status is new Combined_Status_Control(
    Raw_Data => Write_Data_Status_Control,
    Structural => Write_Structural_Stratus_Control,
    Model_Types => Transaction_Model_Types
);

package Byte_Readers is
    use Read_Status;

    procedure Read_Byte(Byte: out Byte_Type)
    with
        Pre => Status_OK,
        Post => (
            if Status_OK
            then (
                Bytes_Processed_Since(Raw_Data_Status'Old) = 1
                and then
                Ghost_Data_Tape = Ghost_Data_Tape'Old'Update(
                    Bytes_Processed_With(Raw_Data_Status'Old) => Byte
                )
            )
            else Bytes_Processed_Since(Raw_Data_Status'Old) = 0
        ),
        Depends => (
            Raw_Data_Status =>+ null, Structural_Status =>+ Raw_Data_Status,
            Byte => Raw_Data_Status, Ghost_Data_Tape =>+ Raw_Data_Status
        ),
        Global => (In_Out => (Raw_Data_Status, Structural_Status, Ghost_Data_Tape));
end Byte_Readers;

generic
    Write_Limit: in Positive;
package Byte_Writers is
    use Write_Status;

    procedure Write_Byte(Byte: in Byte_Type)
    with
        Pre => Status_OK,
        Post => (
            if Status_OK
            then Bytes_Processed_Since(Raw_Data_Status'Old) = 1
            else Bytes_Processed_Since(Raw_Data_Status'Old) = 0
        ),
        Depends => (Raw_Data_Status =>+ null, Structural_Status =>+ Raw_Data_Status, null => Byte),
        Global => (In_Out => (Raw_Data_Status, Structural_Status));
end Byte_Writers;

procedure Test_Transaction
with Pre => Read_Status.Status_OK and Read_Status.Bytes_Processed = 0;

procedure Put_Line_and_Return_Length(The_Line: in String; Result_Length: out Natural)
with
    Pre => The_Line'Length <= 200,
    Post => Result_Length = The_Line'Length;

procedure Put_Line_of_Chars(Char: in Character; Num_Chars: in Natural)
with Pre => Num_Chars <= 200;

end Test_Transactions;
