defmodule Tracker.Manager.EntryTest do
  use ExUnit.Case
  alias Tracker.Manager.Entry
    
  test "reorder_trackers" do
    cases = [
      %{
        in_trackers: [["foo", "bar"], ["baz"]],
        new_head: "baz",
        out_trackers: [["foo", "bar"], ["baz"]],
      },
      %{
        in_trackers: [["foo", "bar"], ["baz"]],
        new_head: "",
        out_trackers: [["foo", "bar"], ["baz"]],
      },
      %{
        in_trackers: [["foo", "bar"], ["baz"]],
        new_head: "bar",
        out_trackers: [["bar", "foo"], ["baz"]],
      },
      %{
        in_trackers: [["foo"], ["bar", "baz"]],
        new_head: "baz",
        out_trackers: [["foo"], ["baz", "bar"]],
      },
    ]

    for %{in_trackers: t, new_head: head, out_trackers: out} <- cases do
      assert Entry.reorder_trackers(%Entry{trackers: t}, head) == %Entry{trackers: out}
    end
  end
end
