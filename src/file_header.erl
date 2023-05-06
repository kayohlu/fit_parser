-module(file_header).

-export([decode/1]).

decode(FileBin) ->
    <<HeaderSize:8/integer-unsigned-little, R/binary>> = FileBin,
    decode(HeaderSize, R).

decode(12 = HeaderSize, FileBin) ->
    <<ProtocolVersion:8/integer-unsigned-little,
      ProfileVersion:16/integer-unsigned-little,
      DataSizeInBytes:32/integer-unsigned-little,
      DataType:32/bitstring-unsigned-little,
      RemainingFileBin/binary>> =
        FileBin,

    {{HeaderSize, ProtocolVersion, ProfileVersion, DataSizeInBytes, DataType, 0},
     DataSizeInBytes,
     RemainingFileBin};
decode(14 = HeaderSize, FileBin) ->
    <<ProtocolVersion:8/integer-unsigned-little,
      ProfileVersion:16/integer-unsigned-little,
      DataSizeInBytes:32/integer-unsigned-little,
      DataType:32/bitstring-unsigned-little,
      Crc:16/integer-unsigned-little,
      RemainingFileBin/binary>> =
        FileBin,
    {{HeaderSize, ProtocolVersion, ProfileVersion, DataSizeInBytes, DataType, Crc},
     DataSizeInBytes,
     RemainingFileBin}.
