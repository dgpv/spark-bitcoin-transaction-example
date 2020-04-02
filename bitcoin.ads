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

package Bitcoin is

Max_Block_Weight: constant Positive := 4000000;

-- limitation in Bitcoin Core's EvalScript()
Max_Script_Size: constant Positive := 10000;

Max_Script_Element_Size: constant Positive := 520;

Max_Standard_Transaction_Weight: constant Positive := 400000;

Max_Standard_P2WSH_Script_Size: constant Positive := 3600;

Max_Standard_P2WSH_Stack_Items: constant Positive := 100;

Max_Standard_P2WSH_Stack_Item_Size: constant Positive := 80;

Max_Standard_Signature_Script_Size: constant Positive := 1650;

-- 83 is a maximum length of OP_RETURN output.
-- Bare multisig outputs can be larger, because they can hold up to 3 pubkeys.
-- Bare multisig outputs are deemed standard outptus, but there is no point
-- in using bare multisig in contemporary transactions, so we do not support it.
-- Therefore the maximum size of standard scriptPubKey will be as with OP_RETURN.
Max_Standard_Pubkey_Script_Size: constant Positive := 83;

Satoshi_In_Coin: constant := 100_000_000;
Max_Money: constant := 21_000_000*Satoshi_In_Coin;

type Satoshi_Type is range 0..Max_Money;

end Bitcoin;
