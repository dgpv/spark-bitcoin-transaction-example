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

with Ada.Text_IO;
with Utils; use Utils;

package Stdio is

package Char_Readers is
    procedure Read_Char(Char: out Character; Has_Read: out Boolean);
end Char_Readers;

end Stdio;
