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

package Bitcoin_Like.Utils is

type Unsigned_Modular_Int16 is mod 2**16;
type Unsigned_Modular_Int32 is mod 2**32;
type Unsigned_Modular_Int64 is mod 2**64;

type Unsigned_Ranged_Int16 is range 0..2**16-1;
type Unsigned_Ranged_Int32 is range 0..2**32-1;

subtype Compact_Size_Type is Natural range 0..16#2000000#;
subtype Size_of_Compact_Size_Type is Natural range 1..9;


function Size_of_Compact_Size(Value: Compact_Size_Type) return Size_of_Compact_Size_Type
is (
        if Value < 16#FD# then 1
        elsif Value <= 16#FFFF# then 3
        elsif Value <= 16#FFFFFFF# then 5
        else 9
);

end Bitcoin_Like.Utils;
