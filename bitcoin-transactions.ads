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

with Bitcoin;

with Bitcoin_Like;
with Bitcoin_Like.Scripts;
with Bitcoin_Like.Transactions;
with Bitcoin_Like.Transaction_Inputs;
with Bitcoin_Like.Transaction_Outputs;
with Bitcoin_Like.Transaction_Input_Witnesses;
with Bitcoin_Like.Data_Accessors;

generic
    with package Data_Readers is new Bitcoin_Like.Data_Accessors.Readers (<>);
    with package Data_Writers is new Bitcoin_Like.Data_Accessors.Writers (<>);
package Bitcoin.Transactions is

-- min. possible size for standard input is
-- 32 bytes txid, 4 bytes prev_in, 1 byte for empty script, 4 bytes for sequence number
Min_Transaction_Input_Size: constant Positive := 41;

-- min. possible size for standard output is
-- 8 bytes value, 1 byte size of scriptPubKey, 1 byte OP_RETURN
Min_Transaction_Output_Size: constant Positive := 10;

Min_Transaction_Auxiliary_Data_Size: Constant Positive :=
    (
        4 -- version_number
        + 5 -- compact size for number of inputs
        + 1 -- compact size for number of outputs (minumal number of outputs)
        + 4 -- lock_time
    );

-- NOTE: The max number of inputs and outputs in a transaction might be too large
-- for the practical cases. These constants influence the size of the records,
-- and therefore influence the amount of memory consumed to hold the transaction
-- record. If memory is limited, values of these constants can be reduced.

Max_Inputs_Per_Transaction: constant Positive :=
    (
        (Bitcoin.Max_Standard_Transaction_Weight / 4) -- non-witness weight is not discounted
        - Min_Transaction_Auxiliary_Data_Size
        - Min_Transaction_Output_Size
    ) / Min_Transaction_Input_Size;

Max_Outputs_Per_Transaction: constant Positive :=
    (
        (Bitcoin.Max_Standard_Transaction_Weight / 4) -- non-witness weight is not discounted
        - Min_Transaction_Auxiliary_Data_Size
        - Min_Transaction_Input_Size
    ) / Min_Transaction_Output_Size;

Max_Inputs_Per_Nonstandard_Transaction: constant Positive :=
    (
        -- non-standard transaction might take the whole block
        (Bitcoin.Max_Block_Weight / 4) -- non-witness weight is not discounted
        - Min_Transaction_Auxiliary_Data_Size
        - Min_Transaction_Output_Size
        - (Min_Transaction_Auxiliary_Data_Size
           + Min_Transaction_Input_Size
           + Min_Transaction_Output_Size) -- min size of coinbase transaction
    ) / Min_Transaction_Input_Size;


type Version_Type is range 1..2;

type Previous_Output_Index_Type is range 0..Max_Inputs_Per_Nonstandard_Transaction;

package P2SH_Scripts is new Bitcoin_Like.Scripts(
    Max_Data_Size => Bitcoin.Max_Script_Element_Size,
    Data_Readers => Data_Readers,
    Data_Writers => Data_Writers
);

package P2WSH_Scripts is new Bitcoin_Like.Scripts(
    Max_Data_Size => Bitcoin.Max_Standard_P2WSH_Script_Size,
    Data_Readers => Data_Readers,
    Data_Writers => Data_Writers
);

package Signature_Scripts is new Bitcoin_Like.Scripts(
    Max_Data_Size => Bitcoin.Max_Standard_Signature_Script_Size,
    Data_Readers => Data_Readers,
    Data_Writers => Data_Writers
);

package Pubkey_Scripts is new Bitcoin_Like.Scripts(
    Max_Data_Size => Bitcoin.Max_Standard_Pubkey_Script_Size,
    Data_Readers => Data_Readers,
    Data_Writers => Data_Writers
);

package Inputs is new Bitcoin_Like.Transaction_Inputs(
    Data_Readers => Data_Readers,
    Data_Writers => Data_Writers,
    Scripts => Signature_Scripts,
    Previous_Output_Index_Type => Previous_Output_Index_Type
);

package Outputs is new Bitcoin_Like.Transaction_Outputs(
    Data_Readers => Data_Readers,
    Data_Writers => Data_Writers,
    Scripts => Pubkey_Scripts,
    Satoshi_Type => Bitcoin.Satoshi_Type
);

package Input_Witnesses is new Bitcoin_Like.Transaction_Input_Witnesses(
    Data_Readers => Data_Readers,
    Data_Writers => Data_Writers,
    Scripts => P2WSH_Scripts,
    Max_Witness_Data_Size => Bitcoin.Max_Standard_P2WSH_Stack_Item_Size,
    Max_Witness_Stack_Items => Bitcoin.Max_Standard_P2WSH_Stack_Items
);

package Transactions is new Bitcoin_Like.Transactions(
    Data_Readers => Data_Readers,
    Data_Writers => Data_Writers,

    Version_Type => Version_Type,
    Inputs => Inputs,
    Outputs => Outputs,
    Input_Witnesses => Input_Witnesses,
    -- It is not practical to put bigger numbers here, because with
    -- maximum allowed script size the memory required becomes too big.
    -- For practical use, dynamic memory allocation should be employed.
    -- But for demostration purposes, this will suffice.
    Max_Inputs => 100,
    Max_Outputs => 100
);

end Bitcoin.Transactions;
