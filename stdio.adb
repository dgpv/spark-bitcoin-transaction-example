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

package body Stdio is

package body Char_Readers is
    procedure Read_Char(Char: out Character; Has_Read: out Boolean) is
    begin
        begin
            Ada.Text_IO.Get(Char);
            Has_Read := True;
        exception
            when Ada.Text_IO.End_Error =>
                Char := Character'Val(0);
                Has_Read := False;
        end;
    end;
end Char_Readers;

end Stdio;
