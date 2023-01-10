# Commits, Pull Requests & Merging

This guide will discuss how the Grain core team handles commit messages, pull requests, and merging.

**Note:** We don't require external contributors to follow these rules, but following these guidelines definitely helps us when accepting contributions.

## Commits

We want to keep our commits small and focused. This allows for easily reviewing individual commits and/or splitting up Pull Requests when they grow too big. Additionally, this allows us to merge smaller changes in quicker and release more often.

When committing, it's often useful to use the `git add -p` workflow to decide on what parts of the changeset to stage for commit. When making the commit, write the commit message as a Conventional Commit.

### Conventional Commits

As of the [6202982](https://github.com/grain-lang/grain/commit/620298225faf35265e7285fe3d4d2c8dee72dba3) commit, Grain follows the [Conventional Commits (v1.0.0)](https://www.conventionalcommits.org/en/v1.0.0/) specification. Following this convention allows us to provide an automated release processs that also generates a detailed changelog.

As described by the spec, our commit messages should be written as:

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

Some examples of this pattern include:

```
feat(stdlib): Implement Char toString (#481)
```

```
feat: add support for LSP lenses (#416)

Introduces a new package `Grain_diagnostics` that provides information needed by Grain's LSP (which used to only contain errors, but now also contains lenses for top-level statements).
```

### Conventional Commits: Types

Generally, we want to scope our types to the three primary types defined by the spec:

- `feat:` - This should be the most used type, as most work we are doing in the project are new features. Commits using this type will always show up in the Changelog.
- `fix:` - When fixing a bug, we should use this type. Commits using this type will always show up in the Changelog.
- `chore:` - The least used type, these are **not** included in the Changelog unless they are breaking changes.

We've used some legacy types previously, but these often have a better format:

- `docs:` - These should be annotated as `chore(docs)` if they aren't being changed with a feature or fix.
- `refactor:` - These should be annotated as `chore(refactor)`.
- `ci:` - These should be annotated as `chore(ci)`.

### Conventional Commits: Breaking Changes

Annotating **BREAKING CHANGES** is extremely important to our release process and versioning. To mark a commit as breaking, we add the `!` character after the type, but before the colon. For example:

```
feat!: Update assignment semantics (#560)
feat(compiler)!: Rework async syntax
```

### Conventional Commits: Scopes

In the past, we weren't very disciplined about using scopes; however, they significantly improve the Changelog, so we want to use a scope whenever possible. If we are only changing one part of the project, we can use the name of the workspace, like `(stdlib)` or `(grainc)`. If a change touches multiple parts of the codebase, there might be a better scope, like `(refactor)`.

```
feat(stdlib): Add Array.reverse() function (#698)
```

```
fix(grainc): Apply separator normalization before cmdliner util (#654)
```

## Pull Requests

Pull requests should be focused on the specific change they are working towards. If additional work is required to finish the original pull request, that work should be submitted as a separate one.

This strategy avoids scenarios where pull requests grow too large/out-of-scope and don't get proper reviews—we want to avoid "LGTM, I trust you" reviews.

The easiest way to do this is to have multiple Conventional Commits while you work and then you can cherry-pick the smaller changes into separate branches for pull requesting.

### Reviews

As the Grain core team has grown, we've adjusted our code review process. On the [grain-lang/grain](https://github.com/grain-lang/grain) repository, we require code approvals as defined by the [CODEOWNERS](/.github/CODEOWNERS) file. **One** codeowner of each file changed in the pull request must approve of the changes before the changes are merged, as enforced by GitHub. Non-breaking pull requests may be merged at any time. Breaking pull requests should only be merged when the core team has general agreement of the changes and is preparing a breaking release. For any other repositories in the grain-lang organization, we require **one** approval from any core team member.

### With Breaking Changes

Sometimes, we don't merge pull requests with breaking changes immediately upon approval. Since a breaking change will cause Grain to bump to the next "minor" version, we might want to land some fixes in "patch" releases before we begin working on that next breaking version.

## Merging

Once approved by the required number of core team members, the submitter can merge their changes into the `main` branch. Sometimes, especially for external contributions, the final approver may merge the pull request instead of the submitter.

We use "squash merging" to merge pull requests. This will cause all commits to be combined into one commit—another reason we want to keep pull requests small & focused.

### Squash Merging

When squash merging, we can keep intermediate Conventional Commits around by adding them to the body of the commit message; however, the GitHub UI adds a `*` character before each commit message and our releaser bot can't parse that.

When squashing, you want to update both the title of the commit to be a good Conventional Commit and adjust the body to contain any other Conventional Commits that you want to keep (not prefixed with `*`) and delete any extra information. We also keep any "Co-authored-by:" lines at the bottom of the commit if the change was done by multiple authors. If "Co-authored-by:" lines appear due to accepted PR suggestions, it's good to delete them so the author gets full credit for the change.

Our overall approach to squashing is to be mindful of the impact of each commit. The commits populate our changelog, so it's important to properly convey to Grain consumers what changes have happened. It is also a record that we and others will review in the future. Thus, we want to attribute the change to its correct authors and provide useful information that future contributors need.

### Merge Checklist

Before merging, you should mentally review these questions:

- Is continuous integration passing?
- Do you have the required amount of approvals?
- Does anyone else need to be pinged for thoughts?
- Will this cause problems for our release schedule? For example: maybe a patch release still needs to be published.
