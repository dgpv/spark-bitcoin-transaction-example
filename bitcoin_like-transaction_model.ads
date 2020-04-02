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

package Bitcoin_Like.Transaction_Model is

type Transaction_Parts_Tag_Type is (
    Tag_Undefined,

    Tag_Version,
    Tag_Witness_Marker,
    Tag_Inputs_Count,
    Tag_Inputs,
    Tag_Outputs_Count,
    Tag_Outputs,
    Tag_Input_Witnesses,
    Tag_Lock_Time,

    Tag_Txid,
    Tag_Prevout_Index,
    Tag_Signature_Script,
    Tag_Sequence_Number,

    Tag_Output_Value,
    Tag_Pubkey_Script,

    Tag_Input_Witness_Stack_Length,
    Tag_Input_Witness_Stack,
    Tag_Input_Witness_Script
);

subtype Transaction_Tag_Type is Transaction_Parts_Tag_Type range Tag_Version..Tag_Lock_Time;
subtype Input_Tag_Type is Transaction_Parts_Tag_Type range Tag_Txid..Tag_Sequence_Number;
subtype Output_Tag_Type is Transaction_Parts_Tag_Type range Tag_Output_Value..Tag_Pubkey_Script;
subtype Input_Witness_Tag_Type is Transaction_Parts_Tag_Type
            range Tag_Input_Witness_Stack_Length..Tag_Input_Witness_Script;

package Transaction_Model_Types is new Data_Controls.Model_Types_Generic(
    Model_Tag_Type => Transaction_Parts_Tag_Type
);

end Bitcoin_Like.Transaction_Model;
