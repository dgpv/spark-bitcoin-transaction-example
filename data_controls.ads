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

package Data_Controls is

generic
    type Model_Tag_Type is (<>);
package Model_Types_Generic is
    subtype Tag_Type is Model_Tag_Type;

    Tag_Undefined_Value: constant Tag_Type := Tag_Type'First;

    function Tag_Undefined(Tag: Tag_Type) return Boolean
    is (Tag = Tag_Undefined_Value);

    subtype Element_Data_Length_Type is Long_Natural range 0..16#2000000#;

    type Element_Type is
        record
            From : Element_Data_Length_Type;
            To   : Element_Data_Length_Type;
        end record;

    function Exact_Length(Length: Element_Data_Length_Type) return Element_Type
    is ((From => Length, To => Length));

    type Model_Type is array (Tag_Type range <>) of Element_Type;

-- This approach is too heavy for the prover, so we had to set
-- model's max serialized data size per-model

--    function Model_Element_Data_Max(Index: Tag_Type; Model: Model_Type)
--        return Long_Natural
--    is (Model(Index).To);
--
--    package Model_Data_Summation is new Array_Summation(
--        State_Type => Model_Type,
--        Element_Size_Max => Natural(Element_Data_Length_Type'Last),
--        Array_Index_Type => Tag_Type,
--        Element_Size_Function => Model_Element_Data_Max
--    );
--
--    function Model_Max_Total_Data_Size(Model: Model_Type) return Long_Natural
--        renames Model_Data_Summation.Sum_of_Element_Sizes;

end Model_Types_Generic;

generic
    type Error_Code_Type is (<>);
package Raw_Data_Status_Control is

    type Status_Type is private;

    function Initial_Status return Status_Type
    with Post => (not Has_Error(Initial_Status'Result));

    procedure Increase_Bytes_Processed(Processed_Count: in Positive; Status: in out Status_Type)
    with
        Pre => not Has_Error(Status) and Bytes_Processed(Status) <= Natural'Last - Processed_Count,
        Post => (
            Bytes_Processed(Status) = Bytes_Processed(Status'Old) + Processed_Count
            and not Has_Error(Status)
        ),
        Depends => (Status => (Processed_Count, Status));

    function Bytes_Processed(Status: in Status_Type) return Natural;

    function Has_Error(Status: Status_Type) return Boolean;

    function Error_Code(Status: Status_Type) return Error_Code_Type
    with
        Pre => Has_Error(Status),
        Post => Error_Code'Result /= Error_Code_Type'First;

    procedure Register_Error(Code: in Error_Code_Type; Status: in out Status_Type)
    with
        Pre => (
            not Has_Error(Status) -- prevent setting error twice
            and then Code /= Error_Code_Type'First -- first must be a 'non-error' code
        ),
        Post => (
            Has_Error(Status)
            and then Error_Code(Status) = Code
            and then Bytes_Processed(Status) = Bytes_Processed(Status'Old)
        ),
        Depends => (Status => (Code, Status));

private

    type Status_Type is
        record
            Bytes_Processed : Natural := 0;
            Error_Code      : Error_Code_Type := Error_Code_Type'First;
        end record;

    function Initial_Status return Status_Type is (others => <>);

    function Bytes_Processed(Status: in Status_Type) return Natural
    is (Status.Bytes_Processed);

    function Has_Error(Status: Status_Type) return Boolean
    is (Status.Error_Code /= Error_Code_Type'First);

    function Error_Code(Status: Status_Type) return Error_Code_Type
    is (Status.Error_Code);

end Raw_Data_Status_Control;

generic
    Max_Error_String_Length: Positive := 128;
package Structural_Status_Control is

    package Error_Strings is new Bounded_Strings (Max_Length => Max_Error_String_Length);
    use Error_Strings;

    type Status_Type is private;

    function Initial_Status return Status_Type
    with Post => (not Has_Error(Initial_Status'Result));

    procedure Register_Error(Message: String; Status: in out Status_Type)
    with
        Pre => (
           not Has_Error(Status) -- prevent setting error twice
           and then Message'Length > 0
           and then Message'Length <= Max_Error_String_Length
           and then Message'Last < Natural'Last - (Max_Error_String_Length - Message'Length)
        ),
        Post => (
            Has_Error(Status)
            and then Error_Message(Status) = Message
        ),
        Depends => (Status =>+ Message);

    function Has_Error(Status: Status_Type) return Boolean;

    function Error_Message(Status: Status_Type) return String
    with
        Pre => Has_Error(Status),
        Post => (
            Error_Message'Result'Length > 0
            and then Error_Message'Result'Length <= Max_Error_String_Length
        );
private

    subtype Error_Message_Type is Error_Strings.Bounded_String;

    type Status_Type is
        record
            -- we need dummy field so that 'in out' parameters work normal
            -- for flow analysis, because otherwise it might get confused when
            -- the only field in the record gets overwrittend inside the procedure,
            -- but we still need it to be 'in out' for checks in preconditions 
            Dummy_Field : Boolean := False;
            Error_Message : Error_Message_Type := Error_Strings.To_Bounded_String("");
        end record;

    function Initial_Status return Status_Type is (others => <>);

    function Has_Error(Status: Status_Type) return Boolean
    is (Error_Strings.Length(Status.Error_Message) > 0);

    function Error_Message(Status: Status_Type) return String
    is (Error_Strings.To_String(Status.Error_Message));

end Structural_Status_Control;

generic
    with package Raw_Data is new Raw_Data_Status_Control (<>);
    with package Structural is new Structural_Status_Control (<>);
    with package Model_Types is new Model_Types_Generic (<>);
package Combined_Status_Control is

    use Model_Types;

    package Data_Checkpointing is

        type State_Type is private;

        function Initial_State return State_Type;

        procedure Register_Data_Tag(Tag: in Tag_Type; State: in out State_Type)
        with
            Pre => not Tag_Undefined(Tag),
            Post => Current_Data_Tag(State) = Tag,
            Depends => (State =>+ Tag);

        procedure Force_Set_Data_Tag(Tag: in Tag_Type; State: in out State_Type)
        with
            Post => Current_Data_Tag(State) = Tag,
            Depends => (State =>+ Tag);

        function Current_Data_Tag(State: in State_Type) return Tag_Type;
    private
        type State_Type is
            record
                -- we need dummy field so that 'in out' parameters work normal
                -- for flow analysis, because otherwise it might get confused when
                -- the only field in the record gets overwrittend inside the procedure,
                -- but we still need it to be 'in out' for checks in preconditions 
                Dummy       : Boolean := false;
                Current_Tag : Tag_Type := Tag_Undefined_Value;
            end record;

        function Initial_State return State_Type is (others => <>);

        function Current_Data_Tag(State: State_Type) return Tag_Type
        is (State.Current_Tag);
    end Data_Checkpointing;

    Raw_Data_Status: Raw_Data.Status_Type := Raw_Data.Initial_Status;
    Structural_Status: Structural.Status_Type := Structural.Initial_Status;
    Data_Tag_State: Data_Checkpointing.State_Type := Data_Checkpointing.Initial_State;

    type Ghost_Data_Tape_Type is array (Natural) of Byte_Type;
    Ghost_Data_Tape: Ghost_Data_Tape_Type with Ghost;

    function Bytes_Processed return Natural
    is (Raw_Data.Bytes_Processed(Raw_Data_Status))
    with Global => (Input => Raw_Data_Status);

    function Bytes_Processed_With(Status: Raw_Data.Status_Type) return Natural
    is (Raw_Data.Bytes_Processed(Status));

    function Bytes_Processed_Since(Status: Raw_Data.Status_Type) return Natural
    is (Bytes_Processed - Bytes_Processed_With(Status))
    with Pre => (Bytes_Processed >= Bytes_Processed_With(Status));

    procedure Increase_Bytes_Processed(Processed_Count: in Positive)
    with
        Pre => Status_OK and Bytes_Processed <= Natural'Last - Processed_Count,
        Post => (
            Bytes_Processed = Bytes_Processed_With(Raw_Data_Status'Old) + Processed_Count
            and Status_OK
        ),
        Depends => (Raw_Data_Status => (Processed_Count, Raw_Data_Status)),
        Global => (In_Out => Raw_Data_Status, Proof_In => Structural_Status);

    function Got_Raw_Data_Error return Boolean
    is (Raw_Data.Has_Error(Raw_Data_Status));

    function Raw_Data_Error_Code return Raw_Data.Error_Code_Type
    is (Raw_Data.Error_Code(Raw_Data_Status))
    with
        Pre => Got_Raw_Data_Error,
        Post => not Raw_Data."=" (Raw_Data_Error_Code'Result, Raw_Data.Error_Code_Type'First);

    procedure Register_Raw_Data_Error(Error_Code: in Raw_Data.Error_Code_Type)
    with
        Pre => (
            Status_OK -- prevent setting error twice
             -- first must be a 'non-error' code
            and then (not Raw_Data."="(Error_Code, Raw_Data.Error_Code_Type'First))
        ),
        Post => (
            Got_Raw_Data_Error
            and then Raw_Data."="(Raw_Data_Error_Code, Error_Code)
            and then Bytes_Processed_Since(Raw_Data_Status'Old) = 0
        ),
        Depends => (Raw_Data_Status => (Error_Code, Raw_Data_Status)),
        Global => (In_Out => Raw_Data_Status, Proof_In => Structural_Status);

    function Structural_Error_Message return String
    is (Structural.Error_Message(Structural_Status))
    with
        Pre => Got_Structural_Error,
        Post => (
            Structural_Error_Message'Result'Length > 0
            and Structural_Error_Message'Result'Length <= Structural.Max_Error_String_Length 
        );

    function Got_Structural_Error return Boolean
    is (Structural.Has_Error(Structural_Status));

    procedure Register_Structural_Error(Message: String)
    with
        Pre => (
            Status_OK -- prevent setting error twice
            and then Message'Length > 0
            and then Message'Length <= Structural.Max_Error_String_Length
            and then Message'Last < Natural'Last - (Structural.Max_Error_String_Length - Message'Length)
        ),
        Post => (
            Got_Structural_Error
            and then Structural_Error_Message = Message
        ),
        Depends => (Structural_Status =>+ Message),
        Global => (In_Out => Structural_Status, Proof_In => Raw_Data_Status);

    procedure Register_Data_Tag(Tag: in Tag_Type)
    with
        Pre => (
            Status_OK
            and not Tag_Undefined(Tag)
        ),
        Post => Current_Data_Tag = Tag,
        Depends => (Data_Tag_State =>+ Tag),
        Global => (In_Out => Data_Tag_State, Proof_In => (Raw_Data_Status, Structural_Status));

    function Current_Data_Tag return Tag_Type
    is (Data_Checkpointing.Current_Data_Tag(Data_Tag_State));

    function Got_Error return Boolean is (Got_Raw_Data_Error or Got_Structural_Error);
    function Status_OK return Boolean is (not Got_Error);

end Combined_Status_Control;

generic
    with package Status_Control is new Combined_Status_Control (<>);

    Model: Status_Control.Model_Types.Model_Type;
package Data_Checkpoints is 

    use Status_Control;
    use Model_Types;

    subtype Model_Range_Type is Tag_Type range Model'First..Model'Last;

    package Checkpoint_States with Ghost is
        type Ghost_State_Type is private;

        procedure Register_Element(
            Tag   : in     Model_Range_Type;
            State : in out Ghost_State_Type
        )
        with
            Pre => (
                Bytes_Processed >= Bytes_Processed_Prev(State)
                and then
                Bytes_Processed - Bytes_Processed_Prev(State) <= Natural(Element_Data_Length_Type'Last)
                and then
                (
                    for all T in Model_Range_Type'Range => (
                        if T < Tag then (
                            Tag_Seen(T, State)
--                            and then (
--                                if T /= Model_Range_Type'First
--                                then Natural(Element_Size(Model_Range_Type'Pred(T), State))
--                                        = Ghost_Tape_Position(T, State)
--                                                - Ghost_Tape_Position(Model_Range_Type'Pred(T), State)
--                            )
                        )
                        else (
                            not Tag_Seen(T, State)
                            and then Element_Size(T, State) = 0
                            and then Ghost_Tape_Position(T, State) = 0
                        )
                    )
                )
            ),
            Post => (
                Bytes_Processed_Prev(State) = Bytes_Processed
                and then
                (
                    for all T in Model_Range_Type'Range => (
                        if T = Tag then (
                            Tag_Seen(T, State)
                            and then
                            Element_Size(T, State) =
                                Long_Natural(Bytes_Processed - Bytes_Processed_Prev(State'Old))
                            and then
                            Ghost_Tape_Position(T, State) = Bytes_Processed_Prev(State'Old)
                        )
                        else (
                            Tag_Seen(T, State) = Tag_Seen(T, State'Old)
                            and then Element_Size(T, State) = Element_Size(T, State'Old)
                            and then Ghost_Tape_Position(T, State) = Ghost_Tape_Position(T, State'Old)
                        )
                    )
                )
                and then
                Bytes_Processed_at_Start(State) = Bytes_Processed_at_Start(State'Old)
            ),
            Depends => (State =>+ (Tag, Raw_Data_Status)),
            Global => (Input => Raw_Data_Status);

        procedure Zap_Element(
            Tag   : in     Model_Range_Type;
            State : in out Ghost_State_Type
        )
        with
            Pre => (
                Bytes_Processed >= Bytes_Processed_Prev(State)
                and then
                (
                    for all T in Model_Range_Type'Range => (
                        if T < Tag then (
                            Tag_Seen(T, State)
    --                      and then (
    --                          if T /= Model_Range_Type'First
    --                          then Natural(Element_Size(Model_Range_Type'Pred(T), State))
    --                              = Ghost_Tape_Position(T, State)
    --                                    - Ghost_Tape_Position(Model_Range_Type'Pred(T), State)
    --                      )
                        )
                        else (
                            not Tag_Seen(T, State)
                            and then Element_Size(T, State) = 0
                            and then Ghost_Tape_Position(T, State) = 0
                        )
                    )
                )
            ),
            Post => (
                Bytes_Processed_Prev(State) = Bytes_Processed_Prev(State'Old)
                and then
                Bytes_Processed_at_Start(State) = Bytes_Processed_at_Start(State'Old)
                and then (
                    for all T in Model_Range_Type'Range => (
                        if T = Tag then (
                            Tag_Seen(T, State)
                            and then Element_Size(T, State) = 0
                            and then Ghost_Tape_Position(T, State) = Bytes_Processed_Prev(State'Old)
                        )
                        else (
                            Tag_Seen(T, State) = Tag_Seen(T, State'Old)
                            and then Element_Size(T, State) = Element_Size(T, State'Old)
                            and then Ghost_Tape_Position(T, State) = Ghost_Tape_Position(T, State'Old)
                        )
                    )
                )
            ),
            Depends => (State =>+ Tag),
            Global => (Proof_In => Raw_Data_Status);

        function Tag_Seen(Tag: Model_Range_Type; State: Ghost_State_Type) return Boolean;

        function Element_Size(Tag: Model_Range_Type; State: Ghost_State_Type) return Element_Data_Length_Type;

        function Ghost_Tape_Position(Tag: Model_Range_Type; State: Ghost_State_Type) return Natural;

        function Bytes_Processed_at_Start(State: Ghost_State_Type) return Natural;

        function Bytes_Processed_Prev(State: Ghost_State_Type) return Natural;

        function Is_Initial_State(State: Ghost_State_Type) return Boolean
        is (
            Bytes_Processed_Prev(State) = Bytes_Processed
            and then
            Bytes_Processed_at_Start(State) = Bytes_Processed
            and then (
                for all T in Model_Range_Type'Range => (
                    not Tag_Seen(T, State)
                    and then
                    Element_Size(T, State) = 0
                    and then
                    Ghost_Tape_Position(T, State) = 0
                )
            )
        ) with Ghost;

        procedure Init_State(State: out Ghost_State_Type)
        with
            Post => Is_Initial_State(State),
            Depends => (State => Raw_Data_Status),
            Global => (Input => Raw_Data_Status);

    private
        Type Checkpoint_Array_Type is array (Model_Range_Type) of Boolean;
        Type Element_Size_Array_Type is array (Model_Range_Type) of Element_Data_Length_Type;
        Type Ghost_Tape_Positions_Array_Type is array (Model_Range_Type) of Natural;
        type Ghost_State_Type is
            record
                Checkpoints              : Checkpoint_Array_Type := (others => False);
                Element_Sizes            : Element_Size_Array_Type := (others => 0);
                Ghost_Tape_Positions     : Ghost_Tape_Positions_Array_Type := (others => 0);
                Bytes_Processed_at_Start : Natural := 0;
                Bytes_Processed_Prev     : Natural := 0;
            end record;

        function Tag_Seen(Tag: Model_Range_Type; State: Ghost_State_Type) return Boolean
        is (State.Checkpoints(Tag));

        function Element_Size(Tag: Model_Range_Type; State: Ghost_State_Type) return Element_Data_Length_Type
        is (State.Element_Sizes(Tag));

        function Ghost_Tape_Position(Tag: Model_Range_Type; State: Ghost_State_Type) return Natural
        is (State.Ghost_Tape_Positions(Tag));

        function Bytes_Processed_at_Start(State: Ghost_State_Type) return Natural
        is (State.Bytes_Processed_at_Start);

        function Bytes_Processed_Prev(State: Ghost_State_Type) return Natural
        is (State.Bytes_Processed_Prev);

    end Checkpoint_States;

    use Checkpoint_States;

    State: Ghost_State_Type with Ghost;

--    package Element_Size_Data_Summation with Ghost is
--        package Summation is new Array_Summation(
--            State_Type => Ghost_State_Type,
--            Element_Size_Max => Natural(Element_Data_Length_Type'Last),
--            Array_Index_Type => Model_Range_Type,
--            Element_Size_Function => Element_Size
--        );
--
--        function Element_Sizes_Sum(State: Ghost_State_Type) return Long_Natural
--        is (Summation.Sum_of_Element_Sizes(State));
--    end Element_Size_Data_Summation;

    -- This is the only non-ghost procedure in this package,
    -- it registers the data tag in Data_Tag_State, which is available at runtime.
    procedure Data_Checkpoint(Tag: in Model_Range_Type)
    with
        Pre => (
            Status_OK
            and then (
                if Tag /= Model_Range_Type'First then (
                    Current_Data_Tag = Model_Range_Type'Pred(Tag)
                    and then Bytes_Processed >= Bytes_Processed_Prev(State)
                    and then (
                        Bytes_Processed - Bytes_Processed_Prev(State) <= Natural(Element_Data_Length_Type'Last)
                    )
                    and then (
                        for all T in Model_Range_Type'Range => (
                            if T < Model_Range_Type'Pred(Tag) then (
                                Tag_Seen(T, State)
--                                and then (
--                                    if T /= Model_Range_Type'First
--                                    then Natural(Element_Size(Model_Range_Type'Pred(T), State))
--                                            = Ghost_Tape_Position(T, State)
--                                                    - Ghost_Tape_Position(Model_Range_Type'Pred(T), State)
--                                )
                            )
                            else (
                                not Tag_Seen(T, State)
                                and then Element_Size(T, State) = 0
                                and then Ghost_Tape_Position(T, State) = 0
                            )
                        )
                    )
                )
           )
        ),
        Post => (
            Status_OK
            and then Current_Data_Tag = Tag
            and then Bytes_Processed_Prev(State) = Bytes_Processed
            and then (
                if Tag = Model_Range_Type'First then (
                    Is_Initial_State(State)
                    and then
                    (
                        for all T in Model'Range => (
                            not Tag_Seen(T, State)
                            and then Element_Size(T, State) = 0
                            and then Ghost_Tape_Position(T, State) = 0
                        )
                    )
                    and then Bytes_Processed_at_Start(State) = Bytes_Processed
                )
                else (
                    Bytes_Processed_at_Start(State) = Bytes_Processed_at_Start(State'Old)
                    and then (
                        for all T in Model_Range_Type'Range => (
                            if T < Model_Range_Type'Pred(Tag) then (
                                Tag_Seen(T, State)
                                and then
                                Element_Size(T, State) = Element_Size(T, State'Old)
                                and then
                                Ghost_Tape_Position(T, State) = Ghost_Tape_Position(T, State'Old)
                            )
                            elsif T = Model_Range_Type'Pred(Tag) then (
                                Tag_Seen(T, State)
                                and then
                                Element_Size(T, State) = Long_Natural(
                                    Bytes_Processed - Bytes_Processed_Prev(State'Old)
                                )
                                and then
                                Ghost_Tape_Position(T, State) = Bytes_Processed_Prev(State'Old)
                            )
                            else (
                                not Tag_Seen(T, State)
                                and then Element_Size(T, State) = 0
                                and then Ghost_Tape_Position(T, State) = 0
                            )
                        )
                    )
                )
            )
        ),
        Depends => (
            State =>+ (Raw_Data_Status, Tag),
            Data_Tag_State =>+ Tag
        ),
        Global => (
            In_Out => (Data_Tag_State, State),
            Input => Raw_Data_Status,
            Proof_In => Structural_Status
        );

    procedure Data_Checkpoint_Skip(Tag: in Model_Range_Type)
    with
        Pre => (
            Status_OK
            and then Tag /= Model_Range_Type'Last
            and then Bytes_Processed >= Bytes_Processed_Prev(State)
            and then (
                Bytes_Processed - Bytes_Processed_Prev(State) <= Natural(Element_Data_Length_Type'Last)
            )
            and then Current_Data_Tag = Tag
            and then (
                for all T in Model_Range_Type'Range => (
                   if T < Tag then (
                       Tag_Seen(T, State)
--                       and then (
--                            if T /= Model_Range_Type'First
--                            then Natural(Element_Size(Model_Range_Type'Pred(T), State))
--                                    = Ghost_Tape_Position(T, State)
--                                        - Ghost_Tape_Position(Model_Range_Type'Pred(T), State)
--                       )
                   )
                   else (
                       not Tag_Seen(T, State)
                       and then Element_Size(T, State) = 0
                       and then Ghost_Tape_Position(T, State) = 0
                   )
               )
            )
        ),
        Post => (
            Status_OK
            and then Current_Data_Tag = Model_Range_Type'Succ(Tag)
            and then (
                if Tag = Model_Range_Type'First
                then Bytes_Processed_Prev(State) = Bytes_Processed
                else Bytes_Processed_Prev(State) = Bytes_Processed_Prev(State'Old)
            )
            and then (
                if Tag = Model_Range_Type'First
                then Bytes_Processed_at_Start(State) = Bytes_Processed
                else Bytes_Processed_at_Start(State) = Bytes_Processed_at_Start(State'Old)
            )
            and then (
                for all T in Model_Range_Type'Range => (
                    if T < Tag then (
                        Tag_Seen(T, State)
                        and then Element_Size(T, State) = Element_Size(T, State'Old)
                        and then Ghost_Tape_Position(T, State) = Ghost_Tape_Position(T, State'Old)
                    )
                    elsif T = Tag then (
                        Tag_Seen(T, State)
                        and then Element_Size(T, State) = 0
                        and then (
                            if T = Model_Range_Type'First
                            then Ghost_Tape_Position(T, State) = Bytes_Processed
                            else Ghost_Tape_Position(T, State) = Bytes_Processed_Prev(State'Old)
                        )
                    )
                    else (
                        not Tag_Seen(T, State)
                        and then Element_Size(T, State) = 0
                        and then Ghost_Tape_Position(T, State) = 0
                    )
                )
            )
        ),
        Depends => (
            State =>+ (Raw_Data_Status, Tag),
            Data_Tag_State =>+ Tag
        ),
        Global => (
            In_Out => (Data_Tag_State, State),
            Input => Raw_Data_Status,
            Proof_In => Structural_Status
        );

    procedure Data_Checkpoint_Final
        with
            Ghost => True,
            Pre => (
                Status_OK
                and then Bytes_Processed >= Bytes_Processed_Prev(State)
                and then Bytes_Processed - Bytes_Processed_Prev(State) <= Natural(Element_Data_Length_Type'Last)
                and then Current_Data_Tag = Model_Range_Type'Last
                and then (
                    for all T in Model_Range_Type'Range => (
                       if T /= Model_Range_Type'Last then (
                           Tag_Seen(T, State)
--                           and then (
--                                if T /= Model_Range_Type'First
--                                then Natural(Element_Size(Model_Range_Type'Pred(T), State))
--                                        = Ghost_Tape_Position(T, State)
--                                            - Ghost_Tape_Position(Model_Range_Type'Pred(T), State)
--                           )
                       )
                       else (
                           not Tag_Seen(T, State)
                           and then Element_Size(T, State) = 0
                           and then Ghost_Tape_Position(T, State) = 0
                       )
                    )
                )
            ),
            Post => (
                Status_OK
                and then Bytes_Processed_Prev(State) = Bytes_Processed
                and then Bytes_Processed_at_Start(State) = Bytes_Processed_at_Start(State'Old)
                and then Current_Data_Tag = Model_Range_Type'Last
                and then (
                    for all T in Model_Range_Type'Range => (
                        Tag_Seen(T, State)
                        and then
                        (
                            if T = Model_Range_Type'Last
                            then (
                                Element_Size(T, State) = Long_Natural(
                                    Bytes_Processed - Bytes_Processed_Prev(State'Old)
                                )
                                and then
                                Ghost_Tape_Position(T, State) = Bytes_Processed_Prev(State'Old)
                            )
                            else (
                                Element_Size(T, State) = Element_Size(T, State'Old)
                                and then
                                Ghost_Tape_Position(T, State) = Ghost_Tape_Position(T, State'Old)
                            )
                        )
                   )
                )
            ),
            Depends => (State =>+ Raw_Data_Status),
            Global => (
                In_Out => State,
                Input => Raw_Data_Status,
                Proof_In => (Structural_Status, Data_Tag_State)
            );

    function Element_Size_Match(Tag: Model_Range_Type; Size: Long_Natural) return Boolean
    is (Element_Size(Tag, State) = Size)
    with
        Ghost => True,
        Pre => Tag_Seen(Tag, State),
        Global => (Input => State);

    function Partial_State_Consistent(
        Actual_Bytes_Processed : Natural;
        Last_Tag               : Model_Range_Type
    ) return Boolean
    is (
        -- check number of bytes processed as counted by the caller
        -- is equal to the number of bytes registered in the state for checkpoints
        Actual_Bytes_Processed = Bytes_Processed - Bytes_Processed_at_Start(State)
        and then -- checkpoints till last tag are touched
        (for all T in Model_Range_Type'First..Last_Tag => Tag_Seen(T, State))
        and then -- checkpoints after last tag not yet touched
        (
            Last_Tag = Model_Range_Type'Last
            or else
            (
                for all T in Model_Range_Type'Succ(Last_Tag)..Model_Range_Type'Last =>
                not Tag_Seen(T, State)
                and then Element_Size(T, State) = 0
                and then Ghost_Tape_Position(T, State) = 0
            )
        )
        and then -- number of bytes processed between checkpoints match the model limits
        (
            for all T in Model_Range_Type'First..Last_Tag =>
                (Model(T).From <= Element_Size(T, State) and Element_Size(T, State) <= Model(T).To)
--                and then
--                (
--                    if T /= Last_Tag
--                    then (
--                        Natural(Element_Size(T, State))
--                            = Ghost_Tape_Position(Model_Range_Type'Succ(T), State) - Ghost_Tape_Position(T, State)
--                    )
--                    else (
--                        Natural(Element_Size(T, State))
--                            = Bytes_Processed - Ghost_Tape_Position(T, State)
--                    )
--                )
        )
    )
    with
        Ghost => True,
        Pre => Status_OK,
        Global => (Input => (State, Raw_Data_Status), Proof_In => Structural_Status);

    function Final_State_Consistent(
        Actual_Bytes_Processed: Natural
    ) return Boolean
    is (Partial_State_Consistent(Actual_Bytes_Processed, Model_Range_Type'Last))
    with
        Ghost => True,
        Pre => Status_OK,
        Global => (Input => (State, Raw_Data_Status), Proof_In => Structural_Status);
end Data_Checkpoints;

end Data_Controls;
