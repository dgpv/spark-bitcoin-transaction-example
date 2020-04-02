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

package body Bitcoin_Like.Data_Accessors is

package body Readers is

    procedure Read_Byte(Byte: out Byte_Type)
    is begin
        Read_Byte_Procedure(Byte);
    end Read_Byte;

    package body Block_Reader is
        procedure Read_Block(Block: out Block_Type)
        is
            Byte: Byte_Type;

            Bytes_Read_at_Start : constant Natural := Bytes_Processed with Ghost;
            Ghost_Data_Tape_Old : Ghost_Data_Tape_Type := Ghost_Data_Tape with Ghost;
        begin
            Block := (others => 0);

            for Index in 1..Block'Length loop

                Read_Byte(Byte);

                if Got_Error then
                    return;
                end if;

                Block(Block'First + (Index - 1)) := Octet_Type(Byte);

                pragma Loop_Invariant (not Got_Error);
                pragma Loop_Invariant (
                    -- Bytes_Processed_Since(Raw_Data_Status'Loop_Entry) = Index
                    Bytes_Processed = Bytes_Processed_With(Raw_Data_Status'Loop_Entry) + Index
                );
--                pragma Loop_Invariant (
--                    for all I in 1..Index =>
--                        Octet_Type(Ghost_Data_Tape(Bytes_Read_at_Start + (I - 1)))
--                            = Block(Block'First + (I - 1))
--                );
                pragma Loop_Invariant (
                    Block_Ghost_Tape_Match(Block(Block'First..Block'First + (Index - 1)), Bytes_Read_at_Start)
                );
                pragma Loop_Invariant (
                    for all I in Ghost_Data_Tape'Range =>
                        (
                            if I < Bytes_Read_at_Start or I >= Bytes_Processed
                            then Ghost_Data_Tape(I) = Ghost_Data_Tape_Old(I)
                        )
                );
            end loop;

            pragma Assert(Bytes_Processed - Bytes_Read_at_Start = Block'Length);
            pragma Assert_And_Cut (
                Status_OK
                and then
                Bytes_Processed - Bytes_Read_at_Start = Block'Length
                and then
                Block_Ghost_Tape_Match(Block, Bytes_Read_at_Start)
                and then
                (
                    for all I in Ghost_Data_Tape'Range =>
                        (
                            if (I < Bytes_Read_at_Start or I >= Bytes_Read_at_Start + Block'Length)
                            then Ghost_Data_Tape(I) = Ghost_Data_Tape_Old(I)
                        )
                )
            );
        end Read_Block;

        procedure Read_Block_Zero_Fill(
            Block  :    out Block_Type;
            Length : in     Positive
        ) is
            Byte: Byte_Type;

            Bytes_Read_at_Start : constant Natural := Bytes_Processed with Ghost;
            Ghost_Data_Tape_Old : Ghost_Data_Tape_Type := Ghost_Data_Tape with Ghost;
        begin
            Block := (others => 0);

            for Index in 1..Length loop

                Read_Byte(Byte);

                if Got_Error then
                    pragma Assert(Bytes_Processed - Bytes_Read_at_Start < Length);
                    return;
                end if;

                Block(Block'First + (Index - 1)) := Octet_Type(Byte);

                pragma Loop_Invariant (not Got_Error);
                pragma Loop_Invariant (
                    -- Bytes_Processed_Since(Raw_Data_Status'Loop_Entry) = Natural'Min(Index, Length)
                    Bytes_Processed = Bytes_Processed_With(Raw_Data_Status'Loop_Entry)
                                            + Natural'Min(Index, Length)
                );
--                pragma Loop_Invariant (
--                    for all I in 1..Index =>
--                        Octet_Type(Ghost_Data_Tape(Bytes_Read_at_Start + (I - 1))) = Block(Block'First + (I - 1))
--                );
--                pragma Loop_Invariant (
--                    Block(Block'First..Block'First + (Index - 1))'Length = Index
--                );
--                pragma Loop_Invariant(Bytes_Read_at_Start <= Natural'Last - Natural'Min(Index, Length));
                pragma Loop_Invariant (
                    Block_Ghost_Tape_Match(Block(Block'First..Block'First + (Index - 1)), Bytes_Read_at_Start)
                );
                pragma Loop_Invariant (
                    for all I in Ghost_Data_Tape'Range =>
                        (
                            if I < Bytes_Read_at_Start or I >= Bytes_Processed
                            then Ghost_Data_Tape(I) = Ghost_Data_Tape_Old(I)
                        )
                );
            end loop;

            pragma Assert (Bytes_Processed - Bytes_Read_at_Start = Length);
            pragma Assert_And_Cut (
                Status_OK
                and then
                Bytes_Processed - Bytes_Read_at_Start = Length
                and then
                Block_Ghost_Tape_Match(Block(Block'First..Block'First + (Length - 1)),
                                       Bytes_Read_at_Start)
                and then
                (
                    for all I in Ghost_Data_Tape'Range =>
                        (
                            if (I < Bytes_Read_at_Start or I >= Bytes_Read_at_Start + Length)
                            then Ghost_Data_Tape(I) = Ghost_Data_Tape_Old(I)
                        )
                )
            );
        end Read_Block_Zero_Fill;

        function Block_Ghost_Tape_Match(Block: Block_Type; Offset: Natural) return Boolean
        is begin
            for Index in 1..Block'Length loop
                if Octet_Type(Ghost_Data_Tape(Offset + (Index - 1))) /= Block(Block'First + (Index - 1)) then
                    return False;
                end if;
                pragma Loop_Invariant (
                    for all I in 1..Index =>
                        Octet_Type(Ghost_Data_Tape(Offset + (I - 1))) = Block(Block'First + (I - 1))
                );
            end loop;
--            pragma Assert (
--                for all I in 1..Block'Length =>
--                    Octet_Type(Ghost_Data_Tape(Offset + (I - 1))) = Block(Block'First + (I - 1))
--            );
            return True;
        end Block_Ghost_Tape_Match;
    end Block_Reader;

    procedure Read_Unsigned_16(Value: out Unsigned_Ranged_Int16) is
        -- Only one byte variable is sufficient, as intermediate value can be
        -- stored in Value output parameter, but this seems to confuse the prover.
        Byte_High : Byte_Type;
        Byte_Low  : Byte_Type;

        Bytes_Read_at_Start : constant Natural := Bytes_Processed with Ghost;
        Ghost_Data_Tape_Old : Ghost_Data_Tape_Type := Ghost_Data_Tape with Ghost;
    begin
        Read_Byte(Byte_Low);

        if Got_Error then
            Value := 0;
            return;
        end if;

        Read_Byte(Byte_High);

        if Got_Error then
            Value := 0;
            return;
        end if;

        Value := Unsigned_Ranged_Int16(Byte_High) * 2**8 + Unsigned_Ranged_Int16(Byte_Low);

        pragma Assert_And_Cut (
            Status_OK
            and then
            Bytes_Processed - Bytes_Read_at_Start = 2
            and then
            Value = (
                Unsigned_Ranged_Int16(Ghost_Data_Tape(Bytes_Read_at_Start))
                + Unsigned_Ranged_Int16(Ghost_Data_Tape(Bytes_Read_at_Start + 1)) * 2**8
            )
            and then
            (
                for all I in Ghost_Data_Tape'Range =>
                    (if (I < Bytes_Read_at_Start or I >= Bytes_Read_at_Start + 2)
                     then Ghost_Data_Tape(I) = Ghost_Data_Tape_Old(I))
            )
        );

    end Read_Unsigned_16;

    procedure Read_Unsigned_32(Value: out Unsigned_Ranged_Int32) is
        U16_Value_High : Unsigned_Ranged_Int16;
        U16_Value_Low  : Unsigned_Ranged_Int16;

        Bytes_Read_at_Start : constant Natural := Bytes_Processed with Ghost;
        Ghost_Data_Tape_Old : Ghost_Data_Tape_Type := Ghost_Data_Tape with Ghost;
    begin

        Read_Unsigned_16(U16_Value_Low);

        if Got_Error then
            Value := 0;
            return;
        end if;

        Read_Unsigned_16(U16_Value_High);

        if Got_Error then
            Value := 0;
            return;
        end if;

        Value := Unsigned_Ranged_Int32(U16_Value_High) * 2**16 + Unsigned_Ranged_Int32(U16_Value_Low);

        pragma Assert_And_Cut (
            Status_OK
            and then
            Bytes_Processed - Bytes_Read_at_Start = 4
            and then
            Value = (
                Unsigned_Ranged_Int32(Ghost_Data_Tape(Bytes_Read_at_Start))
                + Unsigned_Ranged_Int32(Ghost_Data_Tape(Bytes_Read_at_Start + 1)) * 2**8
                + Unsigned_Ranged_Int32(Ghost_Data_Tape(Bytes_Read_at_Start + 2)) * 2**16
                + Unsigned_Ranged_Int32(Ghost_Data_Tape(Bytes_Read_at_Start + 3)) * 2**24
            )
            and then
            (
                for all I in Ghost_Data_Tape'Range =>
                    (if (I < Bytes_Read_at_Start or I >= Bytes_Read_at_Start + 4)
                     then Ghost_Data_Tape(I) = Ghost_Data_Tape_Old(I))
            )
        );

    end Read_Unsigned_32;

    -- NOTE: we cannot have ranged Int63 with GNAT currently,
    -- so have to use modular int for Read_Unsigned_64 even though
    -- we use ranged ints for Read_Unsigned_32 and Read_Unsigned_16
    procedure Read_Unsigned_64(Value: out Unsigned_Modular_Int64) is
        U32_Value_High : Unsigned_Ranged_Int32;
        U32_Value_Low  : Unsigned_Ranged_Int32;

        Bytes_Processed_Old : constant Natural := Bytes_Processed with Ghost;
        Ghost_Data_Tape_Old : Ghost_Data_Tape_Type := Ghost_Data_Tape with Ghost;
    begin

        Read_Unsigned_32(U32_Value_Low);

        if Got_Error then
            Value := 0;
            return;
        end if;

        Read_Unsigned_32(U32_Value_High);

        if Got_Error then
            Value := 0;
            return;
        end if;

        Value := Unsigned_Modular_Int64(U32_Value_High) * 2**32 + Unsigned_Modular_Int64(U32_Value_Low);

        pragma Assert(
            Unsigned_Modular_Int64(U32_Value_Low) = (
                Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old))
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 1)) * 2**8
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 2)) * 2**16
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 3)) * 2**24
            )
        );

        pragma Assert(
            Unsigned_Modular_Int64(U32_Value_High) * 2**32 = (
                Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 4)) * 2**32
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 5)) * 2**40
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 6)) * 2**48
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 7)) * 2**56
            )
        );

        pragma Assert(
            Value = (
                Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old))
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 1)) * 2**8
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 2)) * 2**16
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 3)) * 2**24
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 4)) * 2**32
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 5)) * 2**40
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 6)) * 2**48
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 7)) * 2**56
            )
        );

        pragma Assert_And_Cut (
            Status_OK
            and then
            Bytes_Processed - Bytes_Processed_Old = 8
            and then
            Unsigned64_Ghost_Tape_Match(Value, Bytes_Processed_Old)
--            Value = (
--                Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old))
--                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 1)) * 2**8
--                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 2)) * 2**16
--                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 3)) * 2**24
--                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 4)) * 2**32
--                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 5)) * 2**40
--                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 6)) * 2**48
--                + Unsigned_Modular_Int64(Ghost_Data_Tape(Bytes_Processed_Old + 7)) * 2**56
--            )
            and then
            (
                for all I in Ghost_Data_Tape'Range =>
                    (if (I < Bytes_Processed_Old or I >= Bytes_Processed_Old + 8)
                     then Ghost_Data_Tape(I) = Ghost_Data_Tape_Old(I))
            )
        );
    end Read_Unsigned_64;

    procedure Read_Compact_Size(Size: out Compact_Size_Type) is
        Byte      : Byte_Type;
        U16_Value : Unsigned_Ranged_Int16;
        U32_Value : Unsigned_Ranged_Int32;
        U64_Value : Unsigned_Modular_Int64;

        Bytes_Read_at_Start : constant Natural := Bytes_Processed with Ghost;
        Ghost_Data_Tape_Old : Ghost_Data_Tape_Type := Ghost_Data_Tape with Ghost;
    begin
        Size := 0;

        Read_Byte(Byte);

        if Got_Error then
            return;
        end if;

        if Byte < 16#FD# then
            Size := Compact_Size_Type(Byte);
        elsif Byte = 16#FD# then
            Read_Unsigned_16(U16_Value);
            if Got_Error then
                return;
            end if;
            if U16_Value < 16#FD# then
                Register_Structural_Error(
                    "Non-canonical variable length integer encoding: unexpected value"
                    & Unsigned_Ranged_Int16'Image(U16_Value)
                    & "after prefix byte <253>"
                );
                return;
            end if;

            Size := Compact_Size_Type(U16_Value);
        elsif Byte = 16#FE# then
            Read_Unsigned_32(U32_Value);
            if Got_Error then
                return;
            end if;

            if U32_Value < 16#10000# then
                Register_Structural_Error(
                    "Non-canonical variable length integer encoding: unexpected value"
                    & Unsigned_Ranged_Int32'Image(U32_Value)
                    & "after prefix byte <254>"
                );
                return;
            end if;

            if U32_Value > Unsigned_Ranged_Int32(Compact_Size_Type'Last) then
                Register_Structural_Error(
                    "Non-canonical variable length integer encoding for 32-bit integer: length is"
                    & Unsigned_Ranged_Int32'Image(U32_Value)
                    & "while maximum allowed size is"
                    & Natural'Image(Compact_Size_Type'Last)
                );
                return;
            end if;

            Size := Compact_Size_Type(U32_Value);
        else
            pragma Assert (Byte = 16#FF#);

            Read_Unsigned_64(U64_Value);

            if Got_Error then
                return;
            end if;

            if U64_Value < 16#100000000# then
                Register_Structural_Error(
                    "Non-canonical variable length integer encoding: unexpected value"
                    & Unsigned_Modular_Int64'Image(U64_Value)
                    & "after prefix byte <255>"
                );
                return;
            end if;

            if U64_Value > Unsigned_Modular_Int64(Compact_Size_Type'Last) then
                Register_Structural_Error(
                    "Non-canonical variable length integer encoding for 64-bit integer: size is"
                    & Unsigned_Modular_Int64'Image(U64_Value)
                    & "while maximum allowed size is"
                    & Natural'Image(Compact_Size_Type'Last)
                );
                return;
            end if;

            -- NOTE: with max value representable by compact size that is currently used in
            -- Bitcoin Core, this code will be unreachable
            Size := Compact_Size_Type(U64_Value);
        end if;

        pragma Assert (Status_OK);
        pragma Assert (Bytes_Processed - Bytes_Read_at_Start = Size_of_Compact_Size(Size));
        pragma Assert_And_Cut (
            Status_OK
            and then
            Bytes_Processed - Bytes_Read_at_Start = Size_of_Compact_Size(Size)
            and then
            Compact_Data_Size_Ghost_Tape_Match(Size, Bytes_Read_at_Start)
            and then
            (
                for all I in Ghost_Data_Tape'Range =>
                    (if (I < Bytes_Read_at_Start or I >= Bytes_Read_at_Start + Size_of_Compact_Size(Size))
                     then Ghost_Data_Tape(I) = Ghost_Data_Tape_Old(I))
            )
        );
    end Read_Compact_Size;
end Readers;

package body Writers is
    procedure Write_Byte(
        Byte   : in     Byte_Type
    ) is
    begin
        Write_Byte_Procedure(Byte);
    end Write_Byte;

    package body Block_Writer is
        procedure Write_Block(Block: in Block_Type) is
        begin
            for Index in 1..Block'Length loop

                Write_Byte(
                    Byte_Type(
                        Block(Block'First + (Index - 1))
                    )
                );

                if Got_Error then
                    return;
                end if;

                pragma Loop_Invariant (not Got_Error);
                pragma Loop_Invariant (Bytes_Processed_Since(Raw_Data_Status'Loop_Entry) = Index);
            end loop;
        end Write_Block;
    end Block_Writer;

    procedure Write_Unsigned_16(Value: in Unsigned_Ranged_Int16) is
    begin
        Write_Byte(Byte_Type(Unsigned_Modular_Int16(Value) and 16#FF#));
        if Got_Error then
            return;
        end if;
        Write_Byte(Byte_Type(Value / 2**8));
    end Write_Unsigned_16;

    procedure Write_Unsigned_32(Value: in Unsigned_Ranged_Int32) is
    begin
        Write_Unsigned_16(
            Unsigned_Ranged_Int16(
                Unsigned_Modular_Int16(
                    Unsigned_Modular_Int32(Value) and 16#FFFF#)));
        if Got_Error then
            return;
        end if;
        Write_Unsigned_16(Unsigned_Ranged_Int16(Value / 2**16));
    end Write_Unsigned_32;

    procedure Write_Unsigned_64(Value: in Unsigned_Modular_Int64) is
    begin
        Write_Unsigned_32(
            Unsigned_Ranged_Int32(
                Unsigned_Modular_Int32(Value and 16#FFFFFFFF#)));
        if Got_Error then
            return;
        end if;
        Write_Unsigned_32(Unsigned_Ranged_Int32(Value / 2**32));
    end Write_Unsigned_64;

    procedure Write_Compact_Size(Size: in Compact_Size_Type) is
        Bytes_Written_at_Start : constant Natural := Bytes_Processed with Ghost;
    begin
        if Size < 253 then
            Write_Byte(Byte_Type(Size));
            if Got_Error then
                return;
            end if;
        elsif Size <= 16#FFFF# then
            Write_Byte(253);
            if Got_Error then
                return;
            end if;
            Write_Unsigned_16(Unsigned_Ranged_Int16(Size));
            if Got_Error then
                return;
            end if;
        elsif Size <= 16#FFFFFFF# then
            Write_Byte(254);
            if Got_Error then
                return;
            end if;
            Write_Unsigned_32(Unsigned_Ranged_Int32(Size));
            if Got_Error then
                return;
            end if;
        else
            Write_Byte(255);
            if Got_Error then
                return;
            end if;
            Write_Unsigned_64(Unsigned_Modular_Int64(Size));
            if Got_Error then
                return;
            end if;
        end if;
        pragma Assert (not Got_Error);
        pragma Assert (Bytes_Processed - Bytes_Written_at_Start = Size_of_Compact_Size(Size));
    end Write_Compact_Size;
end Writers;

end Bitcoin_Like.Data_Accessors;
