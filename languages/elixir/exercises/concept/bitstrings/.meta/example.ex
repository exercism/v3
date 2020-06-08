defmodule DNA do
  @moduledoc """
  Example solution for the `bitstrings` exercise.

  Written by Tim Austin, tim@neenjaw.com, June 2020.

   | NucleicAcid | Bits |
   | ----------- | ---- |
   |     A       | 0001 |
   |     C       | 0010 |
   |     G       | 0100 |
   |     T       | 1000 |
   |    " "      | 0000 |
  """

  @pairs [
    {?\s, 0b0000},
    {?A, 0b0001},
    {?C, 0b0010},
    {?G, 0b0100},
    {?T, 0b1000},
  ]

  @pairs
  |> Enum.each(fn {codepoint, code} ->
    def encode_nucleotide(unquote(codepoint)), do: unquote(code)
  end)

  @pairs
  |> Enum.each(fn {codepoint, code} ->
    def decode_nucleotide(unquote(code)), do: unquote(codepoint)
  end)

  def encode(dna) do
    do_encode(dna)
  end

  defp do_encode(dna_list, acc \\ <<>>)
  defp do_encode([], acc), do: acc
  defp do_encode([n | rest], acc) do
    do_encode(rest, <<acc::bitstring, encode_nucleotide(n)::4>>)
  end

  def decode(dna) do
    do_decode(dna)
  end

  def do_decode(dna_bitstring, acc \\ [])
  def do_decode(<<>>, acc), do: acc |> Enum.reverse()
  def do_decode(<<n::4, rest::bitstring>>, acc), do: do_decode(rest, [decode_nucleotide(n) | acc])
end
