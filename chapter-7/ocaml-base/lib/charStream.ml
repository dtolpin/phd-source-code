(* base/lib/charStream.ml

   Copyright (C) 2008 Holger Arnold 

   This file may be redistributed and modified under the terms of the 
   GNU LGPL version 2.1.  See the LICENSE file for details.  
*)

open ExtString

type regexp     = Pcre.regexp
type substrings = Pcre.substrings

type char_stream = {

  block_size:    int;        (** Size of a block in chars. *)
  block_overlap: int;        (** Overlap between blocks in chars. *)
  min_rspace:    int;        (** Minimum space for regexp matching. *)

  length:        int;        (** Length of the stream in chars. *)
  input:         in_channel; (** Input if created from a channel. *)

  buffer:        string;     (** The block buffer. *)
  mutable buffer_pos: int;   (** Stream position of the current block. *)
}

type t = char_stream

(** [read_block s pos length] reads a block of [length] characters from
    current position in the input channel and writes it to the block buffer
    starting at [pos].

    The functions in this module use [read_block] exclusively to fill the
    block buffer.  If the input channel is not modified after creating the
    char stream from it, there will be at least [length] characters left in
    the channel when [read_block] is called, but this condition cannot be
    enforced by this module.  Therefore, an exception is raised if less than
    [length] characters are available.

    @raise Failure if less than [length] characters could be read. 
*)
let read_block s pos length =
  if Io.input s.input s.buffer pos length = length then ()
  else failwith "CharStream.read_block: I/O error"

let from_string str = 
  let len = String.length str in {

      block_size    = len;
      block_overlap = 0;
      min_rspace    = 0;
      
      length        = len;
      input         = Obj.magic 0;

      buffer        = str;
      buffer_pos    = 0;
    }

let from_channel ?(block_size = 1048576) ?block_overlap ?min_rspace chn = 

  let block_overlap = 
    match block_overlap with 
      | Some x -> x
      | None -> block_size / 16 in

  let min_rspace = 
    match min_rspace with 
      | Some x -> x
      | None -> block_size / 64 in

  if block_size < 1 || block_size > Sys.max_string_length then
    invalid_arg "CharStream.from_channel: invalid block size";

  if block_overlap < 1 || block_overlap > block_size / 2 then
    invalid_arg "CharStream.from_channel: invalid block overlap";

  if min_rspace < 1 || min_rspace > block_overlap then
    invalid_arg "CharStream.from_channel: invalid minimum rspace";

  let length       = in_channel_length chn in 
  let block_size   = min block_size length in
  let buffer       = String.create block_size in
  let buffer_pos   = pos_in chn in

  let s = {

    block_size    = block_size;
    block_overlap = block_overlap;
    min_rspace    = min_rspace;

    length        = length;
    input         = chn;

    buffer        = buffer;
    buffer_pos    = buffer_pos;
  } 
  in
    read_block s 0 block_size; 
    s

let length s = 
  s.length

let is_valid_pos s pos = 
  0 <= pos && pos < s.length

let is_visible s pos = 
  s.buffer_pos <= pos && pos < s.buffer_pos + s.block_size

(** [perform_unsafe_seek s pos] sets the position in the input stream to
    [pos], unconditionally reads the corresponding block from the input
    channel, and writes it to the block buffer.  [pos] must be a valid
    position in the input channel.  The function ensures that the block buffer
    contains at least [(max 0 (min (pos - 1) s.block_overlap))] characters
    from the input before [pos] and at least [(max 0 (min (length s - (pos +
    1)) s.block_overlap))] characters from the input after [pos].
*)
let perform_unsafe_seek s pos =
  let new_buffer_pos = 
    min (s.length - s.block_size) (max 0 (pos - s.block_overlap)) in
  let offset = 
    new_buffer_pos - s.buffer_pos 
  in
    if 0 < offset && offset < s.block_size then
      let overlap = s.block_size - offset in
        String.blit s.buffer (s.block_size - overlap) s.buffer 0 overlap;
        seek_in s.input (new_buffer_pos + overlap);
        read_block s overlap (s.block_size - overlap)
    else if offset < 0 && (- offset) < s.block_size then
      let overlap = s.block_size + offset in 
        String.blit s.buffer 0 s.buffer (s.block_size - overlap) overlap;
        seek_in s.input new_buffer_pos;
        read_block s 0 (s.block_size - overlap)
    else begin
      seek_in s.input new_buffer_pos;
      read_block s 0 s.block_size
    end;
    s.buffer_pos <- new_buffer_pos

(** [unsafe_seek s pos] sets the position in the input stream to [pos].  If
    this position is currently not visible, i.e., not covered by the block
    buffer, it reads the corresponding block from the input channel and writes
    it to the block buffer using [perform_unsafe_seek s pos].  [pos] must be a
    valid position in the input channel. 
*)
let unsafe_seek s pos = 
  if not (is_visible s pos) then 
    perform_unsafe_seek s pos

let seek s pos =
  if is_valid_pos s pos then begin
    unsafe_seek s pos
  end else 
    invalid_arg "CharStream.seek: invalid stream position"

let chars_left s pos = 
  if is_valid_pos s pos then
    length s - pos
  else 
    0

let read_char s pos = 
  if is_valid_pos s pos then begin
    unsafe_seek s pos;
    Some (String.unsafe_get s.buffer (pos - s.buffer_pos))
  end else 
    None

let read_string s pos maxlen = 
  if is_valid_pos s pos then 
    let len = min maxlen (chars_left s pos) in
      if is_visible s pos && is_visible s (pos + len - 1) then
        String.sub s.buffer (pos - s.buffer_pos) len
      else if len <= s.block_overlap then begin
        perform_unsafe_seek s pos;
        String.sub s.buffer (pos - s.buffer_pos) len
      end else begin
        let result = String.create len in
        let chars_left = ref len in
        let chars_read = ref 0 in
          seek_in s.input pos;
          while !chars_left > 0 do
            let nchars = min s.block_size !chars_left in
              read_block s 0 nchars;
              String.blit s.buffer 0 result !chars_read nchars;
              chars_left := !chars_left - nchars;
              chars_read := !chars_read + nchars
          done;
          result
      end
  else 
    ""

let match_char s pos c = 
  read_char s pos = Some c

let match_string s pos str = 
  if is_valid_pos s pos then 
    let len = String.length str in
      if len > chars_left s pos then
        false
      else if is_visible s pos && is_visible s (pos + len - 1) then
        String.match_sub s.buffer (pos - s.buffer_pos) str
      else if len <= s.block_overlap then begin
        perform_unsafe_seek s pos;
        String.match_sub s.buffer (pos - s.buffer_pos) str
      end else begin
        let result = ref true in
        let chars_left = ref len in
        let chars_read = ref 0 in
          seek_in s.input pos;
          while !chars_left > 0 do
            let nchars = min s.block_size !chars_left in
              read_block s 0 nchars;
              if String.match_sub2 str !chars_read s.buffer 0 nchars then begin
                chars_left := !chars_left - nchars;
                chars_read := !chars_read + nchars
              end else begin
                result := false;
                chars_left := 0
              end
          done;
          !result
      end
  else 
    str = ""

let pcre_default_flags = 
  Pcre.rflags [`ANCHORED]

let match_regexp s pos rex = 
  if is_valid_pos s pos then begin
    if not (is_visible s pos && is_visible s (pos + s.min_rspace)) then
      perform_unsafe_seek s pos;
    try
      Some (Pcre.exec 
              ~iflags:pcre_default_flags ~rex:rex 
              ~pos:(pos - s.buffer_pos) s.buffer)
    with Not_found ->
      None
  end else
    None

