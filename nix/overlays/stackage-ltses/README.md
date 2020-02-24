# Stackage LTSes

_Reviewed last on 2019-10-18_

The Haskell tool `stack` has the concept of a 'resolver'. This is a set of
Haskell packages fixed at a particular version. The idea is that for the most
part rather than managing versions of individual Haskell packages you manage
a single version of the resolver.

Nix has a similar concept called a 'package set'. It makes one such set
available under `pkgs.haskellPackages`, and there's more under
`pkgs.haskell.packages.ghc**`. Nix also allows you to define your own custom
package sets.

Inside this repo we used `stack` and its resolvers for some time, but since
have switched to letting Haskell dependencies managed by Nix. To manage this
transition without also needing to deal with upgrading Haskell packages, which
might break parts of our code, we want to start using Nix with a package set
that contains exactly the same versions of packages that our `stack` resolver
did. Luckily someone already did the work to make such Nix package sets
available, and its this work we're pulling in here.

With this overlay in place, you will be able to access Nix package sets
mirroring Stack equivalents under `pkgs.haskell.packages.stackage.lts-***`.

The project we're including here seems to no longer be maintained. That's not
a problem for our current resolver, because Stack resolvers once created
don't change. But if at some point we want to upgrade to the latest Stack
resolver, it might not be available from this package.

This isn't a problem. The only reason we're currently using a Nix package set
mirroring a `stack` resolver is, as previously mentioned, to help the change
from using `stack` to using Nix for managing Haskell dependencies. The next time
we upgrade Haskell packages there's no need or reason to stick with a `stack`
based resolver. Instead we can upgrade towards one of Nix's own package sets
found under `pkgs.haskell.packages.ghc**` and remove this overlay.
