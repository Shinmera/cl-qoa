(in-package #:org.shirakumo.qoa)

(docs:define-docs
  (type file
    "Representation of an encoded QOA file.

QOA files contain PCM-encoded audio with a 16-bit signed integer
representation. Each file is composed out of multiple frames of
encoded data, with possibly variable numbers of channels and
samplerates per frame, though this is not typically the case.

See OCTET-SIZE
See SAMPLES/CHANNEL
See FRAMES
See SAMPLERATE
See CHANNELS
See READ-FILE
See WRITE-FILE
See ENCODE-FILE
See ENCODE-FROM-BUFFER
See DECODE-FRAME
See DECODE-TO-BUFFER")

  (type frame
    "Representation of an audio frame as part of a QOA file.

A frame contains the encoding of the sample data, a LMS state buffer,
and an array of encoded audio slices.

See OCTET-SIZE
See SAMPLERATE
See CHANNELS
See DECODE-FRAME
See SAMPLES/CHANNEL
See FILE (type)")

  (function frames
    "Returns the vector of FRAME instances of the FILE

See FILE (type)
See FRAME (type)")
  
  (function octet-size
    "Returns the octet-size of the QOA file or frame if it were written out.

See FILE (type)
See FRAME (type)")

  (function samples/channel
    "Returns the number of samples per channel in the file or frame.

See FILE (type)
See FRAME (type)")
  
  (function samplerate
    "Returns the samplerate of the file or frame in Herz.

See FILE (type)
See FRAME (type)")
  
  (function channels
    "Returns the number of channels of the file or frame.

See FILE (type)
See FRAME (type)")
  
  (function read-file
    "Reads a QOA file from its binary representation.

The STORAGE may be any storage backend supported by binary-structures.

Returns a fresh FILE instance as well as the index at which reading
stopped, if applicable.

See FILE (type)")
  
  (function write-file
    "Writes a QOA file to its binary representation.

The STORAGE may be any storage backend supported by binary-structures.
It must have enough space available to contain the entire file. To
check this beforehand, you can call OCTET-SIZE.

Returns the index at which writing stopped.

See FILE (type)
See OCTET-SIZE")
  
  (function encode-from-buffer
    "Creates a FILE from a buffer of audio samples.

SAMPLES must be a (SIMPLE-ARRAY (SIGNED-BYTE 16) (*)), CHANNELS should
be within [1,8], and SAMPLERATE within [1,16777215].

Returns a fresh FILE instance.

See FILE (type)")
  
  (function encode-file
    "Encode a sample buffer and write it to storage immediately.

OUTPUT may be a storage backend supported by WRITE-FILE, or the symbol
VECTOR, in which case a vector of sufficient size is allocated for you
and returned.

See FILE (type)
See ENCODE-FROM-BUFFER
See WRITE-FILE")
  
  (function decode-frame
    "Decodes a single audio frame to a raw sample buffer.

FRAME must be a FRAME instance, SAMPLES must be a
\(SIMPLE-ARRAY (SIGNED-BYTE 16) (*)) and START and END must be indices
into the SAMPLES array denoting the region to fill.

Returns the index into the SAMPLES array up to which samples were
written, same as CL:READ-SEQUENCE.

If the sample buffer is not big enough to contain the entire frame,
START is returned.

See FRAME (type)")
  
  (function decode-to-buffer
    "Decodes the entire FILE to a raw sample buffer.

FILE must be a FILE, SAMPLES must be a
\(SIMPLE-ARRAY (SIGNED-BYTE 16) (*)) and START and END must be indices
into the SAMPLES array denoting the region to fill. FRAME-START can
designate the first frame of the file that should be decoded.

Returns the index into the SAMPLES array up to which samples were
written, same as CL:READ-SEQUENCE, as well as the index of the next
frame to be decoded.

This assumes that the entire file has a constant channel count and
samplerate.

See FILE (type)
See DECODE-FRAME")
  
  (function decode-file
    "Decodes a QOA file to a sample buffer.

Returns three values:
  The sample buffer,
  The number of channels of audio,
  The samplerate of the audio.

This assumes that the entire file has a constant channel count and
samplerate.

See DECODE-TO-BUFFER
See DECODE-FILE")
  
  (function convert-wav
    "Converts a wav file to a QOA file.

IN should be the path to a RIFF WAVE file with sint16_le encoded audio
samples to convert. OUT should be the path to which the QOA file
should be written.

Returns OUT.

If the source file is not a wave file or does not contain the correct
sample format, an error is signalled.

See ENCODE-FILE")
  
  (function channel-layout
    "Returns the layout of the channels within the packed sample buffer for a given channel count.

The following speaker types are provided:
  :center :left :right :left-front :right-front :subwoofer :left-rear
  :right-rear :left-side :right-side"))
