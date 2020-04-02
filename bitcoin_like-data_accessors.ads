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

with Utils; use Utils;
with Bitcoin_Like.Utils; use Bitcoin_Like.Utils;
with Bitcoin_Like.Transaction_Model; use Bitcoin_Like.Transaction_Model;

package Bitcoin_Like.Data_Accessors is

generic
    with package Status_Control is new Data_Controls.Combined_Status_Control (
        Model_Types => Transaction_Model_Types,
        others => <>
    );
    with procedure Read_Byte_Procedure(Byte: out Byte_Type);
package Readers is

    use Status_Control;

    -- we cannot enforce pre/post conditions on generic subprogram parameter,
    -- but we can wrap it in a procedure that will have the pre/post condtions.
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

    generic
        type Octet_Type is mod <>;
    package Block_Reader is
        type Block_Type is array (Positive range <>) of Octet_Type;

        procedure Read_Block(Block: out Block_Type)
        with
            Pre => Status_OK and Block'Length > 0,
            Post => (
                if Status_OK
                then (
                    Bytes_Processed_Since(Raw_Data_Status'Old) = Block'Length
                    and then
                    Block_Ghost_Tape_Match(Block, Bytes_Processed_With(Raw_Data_Status'Old))
                    and then (
                        for all I in Ghost_Data_Tape'Range => (
                            if (I < Bytes_Processed_With(Raw_Data_Status'Old)
                                or I >= Bytes_Processed_With(Raw_Data_Status'Old) + Block'Length)
                            then Ghost_Data_Tape(I) = Ghost_Data_Tape'Old(I)
                        )
                    )
                )
                else Bytes_Processed_Since(Raw_Data_Status'Old) in 0..Block'Length-1
            ),
            Depends => (
                (Block, Structural_Status, Raw_Data_Status) =>+ (Block, Structural_Status, Raw_Data_Status),
                Ghost_Data_Tape =>+ (Block, Raw_Data_Status, Structural_Status)
            ),
            Global => (In_Out => (Raw_Data_Status, Structural_Status, Ghost_Data_Tape));

        procedure Read_Block_Zero_Fill(
            Block  :    out Block_Type;
            Length : in     Positive
        )
        with
            Pre => Status_OK and Block'Length > 0 and Block'Length >= Length,
            Post => (
                if Status_OK
                then (
                    Bytes_Processed_Since(Raw_Data_Status'Old) = Length
                    and then
                    Block_Ghost_Tape_Match(Block(Block'First..Block'First + (Length - 1)),
                                           Bytes_Processed_With(Raw_Data_Status'Old))
                    and then (
                        for all I in Ghost_Data_Tape'Range => (
                            if (I < Bytes_Processed_With(Raw_Data_Status'Old)
                                or I >= Bytes_Processed_With(Raw_Data_Status'Old) + Length)
                            then Ghost_Data_Tape(I) = Ghost_Data_Tape'Old(I)
                        )
                    )
                )
                else Bytes_Processed_Since(Raw_Data_Status'Old) in 0..Length-1
            ),
            Depends => (
                (Block, Structural_Status, Raw_Data_Status)
                    =>+ (Length, Raw_Data_Status, Structural_Status),
                Ghost_Data_Tape =>+ (Length, Raw_Data_Status, Structural_Status)
            ),
            Global => (In_Out => (Raw_Data_Status, Structural_Status, Ghost_Data_Tape));

        -- XXX spotted potential compiler bug - it was not able to find 'Block_Reader' package
        -- when function was declared like this here:
        -- function Block_Ghost_Tape_Match(Block: Block_Type; Offset: Natural) return Boolean is (True);

        function Block_Ghost_Tape_Match(Block: Block_Type; Offset: Natural) return Boolean
        with Ghost => True,
             Pre => Offset <= Natural'Last - Block'Length,
             Post => (
                 Block_Ghost_Tape_Match'Result
                     = (for all I in 1..Block'Length =>
                            Octet_Type(Ghost_Data_Tape(Offset + (I - 1))) = Block(Block'First + (I - 1)))
             ),
             Global => (Input => Ghost_Data_Tape);
    end Block_Reader;

    function Compact_Data_Size_Ghost_Tape_Match(Size: Compact_Size_Type; Offset: Natural) return Boolean
    is (
        if Size < 16#FD# then
            Offset <= Natural'Last and then Ghost_Data_Tape(Offset) = Byte_Type(Size)
        elsif Size <= 16#FFFF# then
            (
                Offset <= Natural'Last - 2
                and then
                Ghost_Data_Tape(Offset) = 16#FD#
                and then
                Unsigned_Ranged_Int16(Size) = (
                    Unsigned_Ranged_Int16(Ghost_Data_Tape(Offset + 1))
                    + Unsigned_Ranged_Int16(Ghost_Data_Tape(Offset + 2)) * 2**8
                )
            )
        elsif Unsigned_Modular_Int64(Size) <= 16#FFFFFFFF# then
            (
                Offset <= Natural'Last - 4
                and then
                Ghost_Data_Tape(Offset) = 16#FE#
                and then
                Unsigned_Ranged_Int32(Size) = (
                    Unsigned_Ranged_Int32(Ghost_Data_Tape(Offset + 1))
                    + Unsigned_Ranged_Int32(Ghost_Data_Tape(Offset + 2)) * 2**8
                    + Unsigned_Ranged_Int32(Ghost_Data_Tape(Offset + 3)) * 2**16
                    + Unsigned_Ranged_Int32(Ghost_Data_Tape(Offset + 4)) * 2**24
                )
            )
        else
            Offset <= Natural'Last - 8
            and then
            Ghost_Data_Tape(Offset) = 16#FF#
            and then
            Unsigned_Modular_Int64(Size) = (
                Unsigned_Modular_Int64(Ghost_Data_Tape(Offset + 1))
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Offset + 2)) * 2**8
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Offset + 3)) * 2**16
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Offset + 4)) * 2**24
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Offset + 5)) * 2**32
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Offset + 6)) * 2**40
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Offset + 7)) * 2**48
                + Unsigned_Modular_Int64(Ghost_Data_Tape(Offset + 8)) * 2**56
            )
    )
    with Ghost => True,
         Pre => Offset <= Natural'Last - Size_of_Compact_Size(Size),
         Global => (Input => Ghost_Data_Tape);

    procedure Read_Compact_Size(Size: out Compact_Size_Type)
    with
        Pre => Status_OK,
        Post => (
            if Status_OK
            then (
                Bytes_Processed_Since(Raw_Data_Status'Old) = Size_of_Compact_Size(Size)
                and then
                Compact_Data_Size_Ghost_Tape_Match(Size, Bytes_Processed_With(Raw_Data_Status'Old))
                and then
                (
                    for all I in Ghost_Data_Tape'Range => (
                        if (I < Bytes_Processed_With(Raw_Data_Status'Old)
                            or I >= Bytes_Processed_With(Raw_Data_Status'Old) + Size_of_Compact_Size(Size))
                        then Ghost_Data_Tape(I) = Ghost_Data_Tape'Old(I)
                    )
                )
            )
            else Size = 0
        ),
        Depends => (
            Size => (Raw_Data_Status, Structural_Status),
            Raw_Data_Status =>+ Structural_Status,
            Structural_Status =>+ Raw_Data_Status,
            Ghost_Data_Tape =>+ (Raw_Data_Status, Structural_Status)
        ),
        Global => (In_Out => (Raw_Data_Status, Structural_Status, Ghost_Data_Tape));

    procedure Read_Unsigned_16(Value: out Unsigned_Ranged_Int16)
    with
        Pre => Status_OK,
        Post => (
            if Status_OK
            then (
                Bytes_Processed_Since(Raw_Data_Status'Old) = 2
                and then
                Value = (
                    Unsigned_Ranged_Int16(
                        Ghost_Data_Tape(Bytes_Processed_With(Raw_Data_Status'Old)))
                    + Unsigned_Ranged_Int16(
                        Ghost_Data_Tape(Bytes_Processed_With(Raw_Data_Status'Old) + 1)) * 2**8
                )
                and then
                (
                    for all I in Ghost_Data_Tape'Range =>
                        (if (I < Bytes_Processed_With(Raw_Data_Status'Old)
                             or I >= Bytes_Processed_With(Raw_Data_Status'Old) + 2)
                         then Ghost_Data_Tape(I) = Ghost_Data_Tape'Old(I))
                )
            )
            else Bytes_Processed_Since(Raw_Data_Status'Old) in 0..1 and Value = 0
        ),
        Depends => (
            Value => (Raw_Data_Status, Structural_Status),
            (Structural_Status, Raw_Data_Status) =>+ (Raw_Data_Status, Structural_Status),
            Ghost_Data_Tape =>+ (Raw_Data_Status, Structural_Status)
        ),
        Global => (In_Out => (Raw_Data_Status, Structural_Status, Ghost_Data_Tape));

    function Unsigned32_Ghost_Tape_Match(Value: Unsigned_Ranged_Int32; Offset: Natural) return Boolean
    is (
        Value = (
            Unsigned_Ranged_Int32(Ghost_Data_Tape(Offset))
            + Unsigned_Ranged_Int32(Ghost_Data_Tape(Offset + 1)) * 2**8
            + Unsigned_Ranged_Int32(Ghost_Data_Tape(Offset + 2)) * 2**16
            + Unsigned_Ranged_Int32(Ghost_Data_Tape(Offset + 3)) * 2**24
        )
    ) with Ghost => True,
           Pre => Offset <= Natural'Last - 4,
           Global => (Input => Ghost_Data_Tape);

    procedure Read_Unsigned_32(Value: out Unsigned_Ranged_Int32)
    with
        Pre => Status_OK,
        Post => (
            if Status_OK
            then (
                Bytes_Processed_Since(Raw_Data_Status'Old) = 4
                and then
                Unsigned32_Ghost_Tape_Match(Value, Bytes_Processed_With(Raw_Data_Status'Old))
                and then
                (
                    for all I in Ghost_Data_Tape'Range =>
                        (if (I < Bytes_Processed_With(Raw_Data_Status'Old)
                             or I >= Bytes_Processed_With(Raw_Data_Status'Old) + 4)
                         then Ghost_Data_Tape(I) = Ghost_Data_Tape'Old(I))
                )
            )
            else Bytes_Processed_Since(Raw_Data_Status'Old) in 0..3 and Value = 0
        ),
        Depends => (
            Value => (Raw_Data_Status, Structural_Status),
            (Structural_Status, Raw_Data_Status) =>+ (Raw_Data_Status, Structural_Status),
            Ghost_Data_Tape =>+ (Raw_Data_Status, Structural_Status)
        ),
        Global => (In_Out => (Raw_Data_Status, Structural_Status, Ghost_Data_Tape));

    function Unsigned64_Ghost_Tape_Match(Value: Unsigned_Modular_Int64; Offset: Natural) return Boolean
    is (
        Value = (
            Unsigned_Modular_Int64(Ghost_Data_Tape(Offset))
            + Unsigned_Modular_Int64(Ghost_Data_Tape(Offset + 1)) * 2**8
            + Unsigned_Modular_Int64(Ghost_Data_Tape(Offset + 2)) * 2**16
            + Unsigned_Modular_Int64(Ghost_Data_Tape(Offset + 3)) * 2**24
            + Unsigned_Modular_Int64(Ghost_Data_Tape(Offset + 4)) * 2**32
            + Unsigned_Modular_Int64(Ghost_Data_Tape(Offset + 5)) * 2**40
            + Unsigned_Modular_Int64(Ghost_Data_Tape(Offset + 6)) * 2**48
            + Unsigned_Modular_Int64(Ghost_Data_Tape(Offset + 7)) * 2**56
        )
    ) with Ghost => True,
           Pre => Offset <= Natural'Last - 8,
           Global => (Input => Ghost_Data_Tape);

    procedure Read_Unsigned_64(Value: out Unsigned_Modular_Int64)
    with
        Pre => Status_OK,
        Post => (
            if Status_OK
            then (
                Bytes_Processed_Since(Raw_Data_Status'Old) = 8
                and then
                Unsigned64_Ghost_Tape_Match(Value, Bytes_Processed_With(Raw_Data_Status'Old))
                and then
                (
                    for all I in Ghost_Data_Tape'Range => (
                        if (I < Bytes_Processed_With(Raw_Data_Status'Old)
                            or I >= Bytes_Processed_With(Raw_Data_Status'Old) + 8)
                        then Ghost_Data_Tape(I) = Ghost_Data_Tape'Old(I)
                    )
                )
            )
            else Bytes_Processed_Since(Raw_Data_Status'Old) in 0..7 and Value = 0
        ),
        Depends => (
            Value => (Raw_Data_Status, Structural_Status),
            (Structural_Status, Raw_Data_Status) =>+ (Raw_Data_Status, Structural_Status),
            Ghost_Data_Tape =>+ (Raw_Data_Status, Structural_Status)
        ),
        Global => (In_Out => (Raw_Data_Status, Structural_Status, Ghost_Data_Tape));
end Readers;

generic
    with package Status_Control is new Data_Controls.Combined_Status_Control (<>);
    with procedure Write_Byte_Procedure(Byte: in Byte_Type);
package Writers is

    use Status_Control;

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

    generic
        type Octet_Type is mod <>;
    package Block_Writer is
        type Block_Type is array (Positive range <>) of Octet_Type;

        procedure Write_Block(Block: in Block_Type)
        with
            Pre => Status_OK and Block'Length > 0,
            Post => (
                if Status_OK
                then Bytes_Processed_Since(Raw_Data_Status'Old) = Block'Length
                else Bytes_Processed_Since(Raw_Data_Status'Old) in 0..Block'Length-1
            ),
            Depends => (
                Raw_Data_Status =>+ (Block, Structural_Status),
                Structural_Status =>+ (Block, Raw_Data_Status)
            ),
            Global => (In_Out => (Raw_Data_Status, Structural_Status));
    end Block_Writer;

    procedure Write_Compact_Size(Size: in Compact_Size_Type)
    with
        Pre => Status_OK,
        Post => (
            if Status_OK
            then Bytes_Processed_Since(Raw_Data_Status'Old) = Size_of_Compact_Size(Size)
            else Bytes_Processed_Since(Raw_Data_Status'Old) in 0..Size_of_Compact_Size(Size)-1
        ),
        Depends => (Raw_Data_Status =>+ (Size, Structural_Status), Structural_Status =>+ (Size, Raw_Data_Status)),
        Global => (In_Out => (Raw_Data_Status, Structural_Status));

    procedure Write_Unsigned_16(Value: in Unsigned_Ranged_Int16)
    with
        Pre => Status_OK,
        Post => (
            if Status_OK
            then Bytes_Processed_Since(Raw_Data_Status'Old) = 2
            else Bytes_Processed_Since(Raw_Data_Status'Old) in 0..1
        ),
        Depends => ((Raw_Data_Status, Structural_Status) =>+ (Structural_Status, Raw_Data_Status), null => Value),
        Global => (In_Out => (Raw_Data_Status, Structural_Status));

    procedure Write_Unsigned_32(Value: in Unsigned_Ranged_Int32)
    with
        Pre => Status_OK,
        Post => (
            if Status_OK
            then Bytes_Processed_Since(Raw_Data_Status'Old) = 4
            else Bytes_Processed_Since(Raw_Data_Status'Old) in 0..3
        ),
        Depends => ((Raw_Data_Status, Structural_Status) =>+ (Structural_Status, Raw_Data_Status), null => Value),
        Global => (In_Out => (Raw_Data_Status, Structural_Status));

    procedure Write_Unsigned_64(Value: in Unsigned_Modular_Int64)
    with
        Pre => Status_OK,
        Post => (
            if Status_OK
            then Bytes_Processed_Since(Raw_Data_Status'Old) = 8
            else Bytes_Processed_Since(Raw_Data_Status'Old) in 0..7
        ),
        Depends => ((Raw_Data_Status, Structural_Status) =>+ (Structural_Status, Raw_Data_Status), null => Value),
        Global => (In_Out => (Raw_Data_Status, Structural_Status));

end Writers;

end Bitcoin_Like.Data_Accessors;
