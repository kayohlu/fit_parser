% This module decodes the FIT base types.
% Found here: https://developer.garmin.com/fit/protocol/ (Table 7)
-module(fit_field_type_decoder).

-export([decode/4]).

-define(ENUM, 0).
-define(SINT8, 1).
-define(UINT8, 2).
-define(SINT16, 3).
-define(UINT16, 4).
-define(SINT32, 5).
-define(UINT32, 6).
-define(STRING, 7).
-define(FLOAT32, 8).
-define(FLOAT64, 9).
-define(UINT8Z, 10).
-define(UINT16Z, 11).
-define(UINT32Z, 12).
-define(BYTE, 13).
-define(SINT64, 14).
-define(UINT64, 15).
-define(UINT64Z, 16).
-define(ENUM_BYTE_SIZE, 1).
-define(SINT8_BYTE_SIZE, 1).
-define(UINT8_BYTE_SIZE, 1).
-define(SINT16_BYTE_SIZE, 1).
-define(UINT16_BYTE_SIZE, 2).
-define(SINT32_BYTE_SIZE, 4).
-define(UINT32_BYTE_SIZE, 4).
-define(STRING_BYTE_SIZE, 1).
-define(FLOAT32_BYTE_SIZE, 4).
-define(FLOAT64_BYTE_SIZE, 8).
-define(UINT8Z_BYTE_SIZE, 1).
-define(UINT16Z_BYTE_SIZE, 2).
-define(UINT32Z_BYTE_SIZE, 4).
-define(BYTE_BYTE_SIZE, 1).
-define(SINT64_BYTE_SIZE, 8).
-define(UINT64_BYTE_SIZE, 8).
-define(UINT64Z_BYTE_SIZE, 8).
-define(LITTLE_ENDIAN, 0).
-define(BIG_ENDIAN, 1).

decode(0, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 1 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:1/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(0, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 1 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:1/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(1, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 1 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:1/integer-signed-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(1, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 1 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:1/integer-signed-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(2, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 1 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:1/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(2, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 1 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:1/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(3, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 2 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:2/integer-signed-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(3, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 2 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:2/integer-signed-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(4, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 2 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:2/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(4, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 2 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:2/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(5, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 4 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:4/integer-signed-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(5, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 4 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:4/integer-signed-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(6, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 4 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:4/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(6, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 4 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:4/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(7, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    % TODO: Figure out handling string fit field type
    <<FieldValue:FieldSizeInBytes/binary-unit:8, R/binary>> = RecordsBin,
    {FieldValue, R};
decode(7, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    % TODO: Figure out handling string fit field type
    <<FieldValue:FieldSizeInBytes/binary-unit:8, R/binary>> = RecordsBin,
    {FieldValue, R};
decode(8, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 4 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:4/float-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(8, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 4 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:4/float-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(9, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 8 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:8/float-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(9, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 8 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:8/float-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(10, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 1 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:1/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(10, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 1 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:1/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(11, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 2 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:2/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(11, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 2 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:2/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(12, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 4 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:4/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(12, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 4 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:4/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(13, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 1 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:1/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(13, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 1 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:1/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(14, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 8 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:8/integer-signed-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(14, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 8 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:8/integer-signed-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(15, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 8 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:8/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(15, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 8 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:8/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(16, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 0 ->
    case FieldSizeInBytes > 8 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:8/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end;
decode(16, ArchitectureType, FieldSizeInBytes, RecordsBin) when ArchitectureType =:= 1 ->
    case FieldSizeInBytes > 8 of
        true ->
            <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
            {FieldValue, R};
        false ->
            <<FieldValue:8/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
            {FieldValue, R}
    end.

% decode(?ENUM, ArchitectureType, FieldSizeInBytes, RecordsBin)
%     case FieldSizeInBytes > ?ENUM_BYTE_SIZE of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             case ArchitectureType of
%               ?LITTLE_ENDIAN ->
%                 <<FieldValue:?ENUM_BYTE_SIZE/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
%                 {FieldValue, R};
%               ?BIG_ENDIAN ->
%                 <<FieldValue:?ENUM_BYTE_SIZE/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
%                 {FieldValue, R}
%             end
%     end;

% decode(?ENUM, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 1 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?ENUM_BYTE_SIZE/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?ENUM, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 1 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?ENUM_BYTE_SIZE/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?SINT8, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 1 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?SINT8_BYTE_SIZE/integer-signed-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?SINT8, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 1 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?SINT8_BYTE_SIZE/integer-signed-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT8, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 1 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT8_BYTE_SIZE/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT8, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 1 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT8_BYTE_SIZE/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?SINT16, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 2 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?SINT16_BYTE_SIZE/integer-signed-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?SINT16, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 2 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?SINT16_BYTE_SIZE/integer-signed-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT16, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 2 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT16_BYTE_SIZE/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT16, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 2 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT16_BYTE_SIZE/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?SINT32, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 4 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?SINT32_BYTE_SIZE/integer-signed-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?SINT32, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 4 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?SINT32_BYTE_SIZE/integer-signed-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT32, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 4 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT32_BYTE_SIZE/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT32, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 4 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT32_BYTE_SIZE/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?STRING, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     % TODO: Figure out handling string fit field type
%     <<FieldValue:FieldSizeInBytes/binary-unit:8, R/binary>> = RecordsBin,
%     {FieldValue, R};
% decode(?STRING, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     % TODO: Figure out handling string fit field type
%     <<FieldValue:FieldSizeInBytes/binary-unit:8, R/binary>> = RecordsBin,
%     {FieldValue, R};
% decode(?FLOAT32, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 4 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?FLOAT32_BYTE_SIZE/float-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?FLOAT32, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 4 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?FLOAT32_BYTE_SIZE/float-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?FLOAT64, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 8 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?FLOAT64_BYTE_SIZE/float-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?FLOAT64, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 8 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?FLOAT64_BYTE_SIZE/float-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT8Z, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 1 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT8Z_BYTE_SIZE/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT8Z, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 1 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT8Z_BYTE_SIZE/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT16Z, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 2 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT16Z_BYTE_SIZE/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT16Z, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 2 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT16Z_BYTE_SIZE/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT32Z, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 4 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT32Z_BYTE_SIZE/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT32Z, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 4 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT32Z_BYTE_SIZE/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?BYTE, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 1 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?BYTE_BYTE_SIZE/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?BYTE, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 1 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?BYTE_BYTE_SIZE/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?SINT64, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 8 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?SINT64_BYTE_SIZE/integer-signed-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?SINT64, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 8 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?SINT64_BYTE_SIZE/integer-signed-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT64, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 8 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT64_BYTE_SIZE/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT64, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 8 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT64_BYTE_SIZE/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT64Z, ?LITTLE_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 0 ->
%     case FieldSizeInBytes > 8 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT64Z_BYTE_SIZE/integer-unsigned-little-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end;
% decode(?UINT64Z, ?BIG_ENDIAN = ArchitectureType, FieldSizeInBytes, RecordsBin)
%     when ArchitectureType =:= 1 ->
%     case FieldSizeInBytes > 8 of
%         true ->
%             <<FieldValue:FieldSizeInBytes/binary, R/binary>> = RecordsBin,
%             {FieldValue, R};
%         false ->
%             <<FieldValue:?UINT64Z_BYTE_SIZE/integer-unsigned-big-unit:8, R/binary>> = RecordsBin,
%             {FieldValue, R}
%     end.
