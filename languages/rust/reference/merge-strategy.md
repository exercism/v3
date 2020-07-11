# merging strategy

The Rust track maintainers [have decided](https://github.com/exercism/v3/discussions/1725#discussion-7438) to write down our merging strategy to communicate expectations and encourage quality contributions.

## general principles
- we strive to accept *all* contributions even when **not** perfect.
    - We adopt this principle of optimistic merging / Google engineering's code review handbook. See our [original discussion](https://github.com/exercism/v3/discussions/1725#discussion-7438) for appropriate sources.
- One other maintainer should review a contributor's pull request
- At least one other maintainer should approve a pull request before merging it into the development branch

## pull request workflow
### definitions
| Term        |                                       Meaning |
|-------------|----------------------------------------------:|
| Contributor |          anyone adding a new concept exercise |
| Maintainer  | Anyone who belongs to the [@exercism/rust team](https://github.com/orgs/exercism/teams/rust) |
| development branch |          the [v3](https://github.com/exercism/v3) repository's default development branch. Currently `master`. |

### the workflow
1. Contributors start by opening a [draft pull request](https://github.blog/2019-02-14-introducing-draft-pull-requests/) with their changes
    2. Our track follows broader exercism community here. Using the draft feature signals the work is ready for high-level review.
3.  One other maintainer shall review the contribution and provide feedback.
4.  The contributor shall address essential feedback as part of the pull request.
5.  The reviewer shall create a GitHub issue for tangential feedback not addressed in the original pull request.
    6.  The reviewer will lessen the contributor workload and preserve momentum by taking this step.
7. The maintainer shall review the feedback and either:
    8. approve
    9. request additional changes. At this point, the process returns to step 3.
10. Upon approval, the contributor shall merge their pull request.

