You've been asked to do some more work on the network authentication system.

In addition to the admin identity being hard-coded in the system during development the powers that be also want senior developers to be given the same treatment.

## 1. Store the system admin's details hard-coded in the system and make it available to callers.

The admin's details are as follows: email address is..., eye color...

Implement `Authenticator.GetAdmin()` to return the system admin's identity details.

```csharp
var authenticator = new Authenticator();
authentiator.GetAdmin();
// => {"admin@exerc.ism", {"green", 0.9m}, ["Chanakya Niti Kautilya Arthashastra", "Plausible Address", "Mombai"]}
```

## 2 Store the developers' details hard-coded in the system and make them available in the form of a dictionary

Implement `Authenticator.GetAdmin()` to return the system admin's identity details.

Table of developer details - 2 developers

```csharp
var authenticator = new Authenticator();
authentiator.GetDevelopers();
// => {"bert" = {"bert@exerc.ism", {"green", 0.9m}, ["Bertrand Meyer", "Avenue des Champs-Élysées", "Paris"]},
// ["anders" = ...

```
