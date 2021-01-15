defmodule Poker do
  import String, only: [capitalize: 1, codepoints: 1, to_atom: 1, to_integer: 1, at: 2, split: 2]
  import Enum, only: [reverse: 1, map: 2, join: 2, sort: 1, sort_by: 2]
  import List, only: [to_tuple: 1]
  import Tuple, only: [to_list: 1]

  def deal([a|[b|[c|[d|flop]]]]) do
    parse_input = &(&1 |> map(fn n -> at("A23456789TJQK", rem(n - 1, 13)) <> at("cdhs", div(n - 1, 13)) end) |> join(" "))
    format_output = &(&1 |> to_list |> map(fn {r, s} -> to_string(if card_value(r) == 14, do: 1, else: card_value(r)) <> capitalize(to_string(s)) end))
    hand1 = best_hand(parse_input.([a, c]), parse_input.(flop))
    hand2 = best_hand(parse_input.([b, d]), parse_input.(flop))
    format_output.(if hand_compare(hand1, hand2) == :gt, do: hand1, else: hand2) |> sort
  end

  defp best_hand(hand, flop) when is_binary(hand) do
    best_hand(parse_hand(hand), flop)
  end
  defp best_hand(hand, flop) when is_binary(flop) do
    best_hand(hand, parse_hand(flop))
  end
  defp best_hand(hand, flop) do
    comb(5, to_list(hand) ++ to_list(flop))
    |> sort_by(fn cards ->
      cards |> to_tuple |> hand_value
    end)
    |> reverse
    |> hd
    |> to_tuple
    |> sort_hand
  end

  defp comb(0, _), do: [[]]
  defp comb(_, []), do: []
  defp comb(m, [h|t]) do
    (for l <- comb(m-1, t), do: [h|l]) ++ comb(m, t)
  end

  defp hand_compare(hand1, hand2) when is_binary(hand1) do
    hand_compare(parse_hand(hand1), hand2)
  end
  defp hand_compare(hand1, hand2) when is_binary(hand2) do
    hand_compare(hand1, parse_hand(hand2))
  end
  defp hand_compare(hand1, hand2) do
    r = hand_value(hand1) - hand_value(hand2)

    cond do
      r > 0  -> :gt
      r == 0 -> :eq
      r < 0  -> :lt
    end
  end

  defp hand_value(str) when is_binary(str) do
    str |> parse_hand |> hand_value
  end
  defp hand_value(hand) do
    case hand_rank(hand) do
      {:straight_flush, a}        -> 8_000 + card_value(a)
      {:four_of_a_kind, _a, b}    -> 7_000 + card_value(b)
      {:full_house, a, b}         -> 6_000 + 15 * card_value(a) + card_value(b)
      {:flush, _r, a, b, c, d, e} -> 5_000 + card_value(a) + card_value(b) + card_value(c) + card_value(d) + card_value(e)
      {:straight, a}              -> 4_000 + card_value(a)
      {:three_of_a_kind, a, b, c} -> 3_000 + 15 * card_value(a) + card_value(b) + card_value(c)
      {:two_pair, a, b, c}        -> 2_000 + 15 * card_value(a) + 15 * card_value(b) + card_value(c)
      {:one_pair, a, b, c, d}     -> 1_000 + 15 * card_value(a) + card_value(b) + card_value(c) + card_value(d)
      {:high_card, a, b, c, d, e} -> card_value(a) + card_value(b) + card_value(c) + card_value(d) + card_value(e)
    end
  end

  defp hand_rank(str) when is_binary(str) do
    parse_hand(str) |> hand_rank
  end
  defp hand_rank(hand) do
    hand = sort_hand(hand)
    if is_straight(hand) do
      {{r1,_}, {r2,_}, _, _, _} = hand
      r = if r1 == :A && r2 == 5, do: 5, else: r1
      if is_flush(hand), do: {:straight_flush, r}, else: {:straight, r}
    else
      case hand do
        {{a,_},  {a,_},  {a,_},  {a,_},  {b,_}}  -> {:four_of_a_kind, a, b}
        {{b,_},  {a,_},  {a,_},  {a,_},  {a,_}}  -> {:four_of_a_kind, a, b}
        {{a,_},  {a,_},  {a,_},  {b,_},  {b,_}}  -> {:full_house, a, b}
        {{b,_},  {b,_},  {a,_},  {a,_},  {a,_}}  -> {:full_house, a, b}
        {{r1,a}, {r2,a}, {r3,a}, {r4,a}, {r5,a}} -> {:flush, a, r1, r2, r3, r4, r5}
        {{a,_},  {a,_},  {a,_},  {b,_},  {c,_}}  -> {:three_of_a_kind, a, b, c}
        {{b,_},  {a,_},  {a,_},  {a,_},  {c,_}}  -> {:three_of_a_kind, a, b, c}
        {{b,_},  {c,_},  {a,_},  {a,_},  {a,_}}  -> {:three_of_a_kind, a, b, c}
        {{a,_},  {a,_},  {b,_},  {b,_},  {c,_}}  -> {:two_pair, a, b, c}
        {{a,_},  {a,_},  {c,_},  {b,_},  {b,_}}  -> {:two_pair, a, b, c}
        {{c,_},  {a,_},  {a,_},  {b,_},  {b,_}}  -> {:two_pair, a, b, c}
        {{a,_},  {a,_},  {b,_},  {c,_},  {d,_}}  -> {:one_pair, a, b, c, d}
        {{b,_},  {a,_},  {a,_},  {c,_},  {d,_}}  -> {:one_pair, a, b, c, d}
        {{b,_},  {c,_},  {a,_},  {a,_},  {d,_}}  -> {:one_pair, a, b, c, d}
        {{b,_},  {c,_},  {d,_},  {a,_},  {a,_}}  -> {:one_pair, a, b, c, d}
        {{a,_},  {b,_},  {c,_},  {d,_},  {e,_}}  -> {:high_card, a, b, c, d, e}
      end
    end
  end

  defp is_straight(str) when is_binary(str) do
    str |> parse_hand |> is_straight
  end
  defp is_straight({{a,_}, {b,_}, {c,_}, {d,_}, {e,_}}) do
    (card_value(a) == card_value(b) + 1 || a == :A && b == 5) &&
      card_value(b) == card_value(c) + 1 &&
      card_value(c) == card_value(d) + 1 &&
      card_value(d) == card_value(e) + 1
  end

  defp is_flush({{_,a},{_,a},{_,a},{_,a},{_,a}}), do: true
  defp is_flush({_,_,_,_,_}),                     do: false

  defp card_value(:A), do: 14
  defp card_value(:K), do: 13
  defp card_value(:Q), do: 12
  defp card_value(:J), do: 11
  defp card_value(:T), do: 10
  defp card_value(i) when is_integer(i) and i >= 2 and i <= 9, do: i

  defp parse_hand(str) do
    str
    |> split(" ")
    |> map(&parse_card/1)
    |> to_tuple
  end

  defp parse_card(str) do
    [rank, suit] = codepoints(str)
    {parse_rank(rank), to_atom(suit)}
  end

  defp parse_rank("A"), do: :A
  defp parse_rank("K"), do: :K
  defp parse_rank("Q"), do: :Q
  defp parse_rank("J"), do: :J
  defp parse_rank("T"), do: :T
  defp parse_rank(str), do: to_integer(str)

  defp sort_hand(hand) do
    hand
    |> to_list
    |> sort_by(fn {rank,_} -> card_value(rank) end)
    |> reverse
    |> to_tuple
  end
end
