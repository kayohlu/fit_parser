% Links to help with parsing binaries in Erlang
% https://cheatography.com/fylke/cheat-sheets/erlang-binaries/
% https://www.erlang.org/doc/programming_examples/bit_syntax.html
% https://www.erlang.org/doc/efficiency_guide/binaryhandling.html
% https://rhye.org/post/erlang-binary-matching-performance/
-module(fit_parser).

-export([parse/1, map/2, filter/2]).

-define(DATA_MESSAGE, 0).
-define(DEFINITION_MESSAGE, 1).

parse(FilePath) ->
    handle_read_file(file:read_file(FilePath)).

handle_read_file({ok, Data}) ->
    {ok, decode_file(Data)};
handle_read_file({error, Reason}) ->
    {error, Reason}.

decode_file(FileBin) ->
    {DecodedHeader, RecordsSizeInBytes, RemainderOfFileBin} = file_header:decode(FileBin),

    <<RecordsBinary:RecordsSizeInBytes/binary-unit:8, Crc:16/integer-unsigned-little>> =
        RemainderOfFileBin,

    decode_records(RecordsBinary).

decode_records(RecordsBinary) ->
    decode_records(#{}, RecordsBinary, []).

decode_records(DefinitionMessagesForFile, <<>>, Records) ->
    Records;
decode_records(DefinitionMessagesForFile, RecordsBin, Acc) ->
    <<RecordHeaderBin:1/binary-unit:8, Rest/binary>> = RecordsBin,

    DecodedRecordHeader = decode_record_header(RecordHeaderBin),

    #{"message_type" := MessageType} = DecodedRecordHeader,

    {UpdatedDefinitionMessagesForFile, Message, RemainingRecordsBin} =
        decode_record_content(MessageType, DefinitionMessagesForFile, DecodedRecordHeader, Rest),

    decode_records(UpdatedDefinitionMessagesForFile, RemainingRecordsBin, [Message | Acc]).

decode_record_header(RecordHeaderBin) ->
    <<HeaderType:1,
      MessageType:1,
      MessaageTypeSpecific:1,
      ReservedBits:1,
      LocalMessageType:4>> =
        RecordHeaderBin,

    #{"header_type" => HeaderType,
      "message_type" => MessageType,
      "messaage_type_specific" => MessaageTypeSpecific,
      "reserved_bits" => ReservedBits,
      "local_message_type" => LocalMessageType}.

decode_record_content(?DATA_MESSAGE,
                      DefinitionMessagesForFile,
                      DecodedRecordHeader,
                      Rest) ->
    DefinitionMessage =
        maps:get(
            maps:get("local_message_type", DecodedRecordHeader), DefinitionMessagesForFile, #{}),

    {DecodedRecordContent, RemainingRecordsBin} =
        data_message:decode(DefinitionMessage, Rest),

    GlobalMessageNumber = definition_message:get(DefinitionMessage, "global_message_number"),

    DecoratedRecordHeader =
        data_message:decorate_header("global_message_number",
                                     GlobalMessageNumber,
                                     DecodedRecordHeader),

    {DefinitionMessagesForFile,
     {DecoratedRecordHeader, DecodedRecordContent},
     RemainingRecordsBin};
decode_record_content(?DEFINITION_MESSAGE,
                      DefinitionMessagesForFile,
                      DecodedRecordHeader,
                      Rest) ->
    {DecodedRecordContent, RemainingRecordsBin} =
        definition_message:decode(DecodedRecordHeader, Rest),

    UpdatedDefinitionMessagesForFile =
        maps:put(
            maps:get("local_message_type", DecodedRecordHeader),
            {DecodedRecordHeader, DecodedRecordContent},
            DefinitionMessagesForFile),

    {UpdatedDefinitionMessagesForFile,
     {DecodedRecordHeader, DecodedRecordContent},
     RemainingRecordsBin}.

map(Fun, DecodedFitFile) ->
    FilteredFile =
        lists:filter(fun(Record) ->
                        {Header, _Content} = Record,
                        #{"message_type" := MessageType} = Header,
                        MessageType =:= ?DATA_MESSAGE
                     end,
                     DecodedFitFile),
    lists:map(Fun, FilteredFile).

filter(Fun, DecodedFitFile) ->
    FilteredFile =
        lists:filter(fun(Record) ->
                        {Header, _Content} = Record,
                        #{"message_type" := MessageType} = Header,
                        MessageType =:= ?DATA_MESSAGE
                     end,
                     DecodedFitFile),
    lists:filter(Fun, FilteredFile).
