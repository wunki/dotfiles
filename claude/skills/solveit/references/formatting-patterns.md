# Formatting Patterns

## Hidden Solutions

Always hide solutions so the reader tries first:

```markdown
<details>
<summary>Solution</summary>

```elixir
# 1-3 lines max
```

</details>
```

## Learning Moments

Use blockquotes for pause-and-explore moments:

```markdown
> **⏸️ Stop if this is new.** The pipe operator `|>` passes the left result as the first argument to the right.
>
> Try it:
> ```
> iex> "hello" |> String.upcase()
> ```
>
> Understand it before continuing.
```

## Code Increment Size

**Too big (don't do this):**
```elixir
def create_site(%Scope{user: user}, attrs \\ %{}) do
  %Site{user_id: user.id}
  |> Site.changeset(attrs)
  |> Repo.insert()
end
```

**Right size (do this):**

First increment:
```elixir
%Site{user_id: user.id}
```
Verify.

Second increment:
```elixir
|> Site.changeset(attrs)
```
Verify.

Third increment:
```elixir
|> Repo.insert()
```
Verify.

Three increments, not one. This is smaller than feels natural. That's the point.

## Verification Commands

Every increment needs verification with exact command and expected output:

```markdown
**Verify immediately:**
```
iex> Site.changeset(%Site{}, %{title: "Test"})
#Ecto.Changeset<action: nil, changes: %{title: "Test"}, ...>
```

✓ Does yours match? If not, debug now. Don't continue with broken code.
```

## File References

Always reference actual files with line numbers:

```markdown
Look at how `lib/app/accounts.ex:27-29` exposes `get_user_by_email/1`.
```

Never give generic advice. Everything relates to THIS codebase.
