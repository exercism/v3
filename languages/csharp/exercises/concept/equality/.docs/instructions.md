You are working on a system to simplify the login process for some network. The tasks concern the authentication part. The system uses facial recognition to prove identity.

In all occurrences the eye color parameter is guaranteed to be non-null.

## 1. Authenticate the system administrator

Despite your protests the system administrator insists on having a built-in identity during acceptance testing so that they can always be authenticated.

The admin's email is admin@exerc.ism. They have green eyes and a philtrum with a width of 0.9.

Add equality routines for the `Identity` and `FacialFeatures` classes.

Implement `Authenticator.IsAdmin()` to check that the identity passed in matches that of the administrator.

```csharp
var authenticator = new Authenticator();
authenticator.IsAdmin(new Identity("admin@exerc.ism", new FacialFeatures("green", 0.9m)));
// => true
authenticator.IsAdmin(new Identity("admin@thecompetition.com", new FacialFeatures("green", 0.9m)));
// => false
```

## 2. Prevent invalid identities being authenticated

Implement `Authenticator.IsRegistered()` and ensure it returns false when no identities have been registered.

```csharp
var authenticator = new Authenticator();
authenticator.IsRegistered(new Identity("alice@thecompetition.com", new FacialFeatures("blue", 0.8m)));
// => false
```

## 3. Register new identities

Implement `Authenticator.Register()` which stores an identity on the authenticator itself such that calls to `IsRegistered()` will return true for this identity. If the identity has already been registered then `false` is returned otherwise `true`.

```csharp
var authenticator = new Authenticator();
authenticator.Register(new Identity("tunde@thecompetition.com", new FacialFeatures("blue", 0.9m)));
// => true
authenticator.IsRegistered(new Identity("tunde@thecompetition.com", new FacialFeatures("blue", 0.9m)));
// => true
authenticator.Register(new Identity("tunde@thecompetition.com", new FacialFeatures("blue", 0.9m)));
// => false

```

## 4. Add diagnostics to detect multiple attempts to authenticate

A bug has been reported whereby `Authenticator.IsRegistered()` is called multiple times in quick succession for the same identity. You believe that there is some sort of "bounce" problem where the exact same record is being submitted multiple times. Your task is to add a diagnostic routine `Authenticator.AreSameObject() to support any testing that's undertaken.

```csharp
var identityA = new Identity("alice@thecompetition.com", new FacialFeatures("blue", 0.9m));
Authenticator.AreSameObject(identityA, identityA);
// => true

var identityB = new Identity("alice@thecompetition.com", new FacialFeatures("blue", 0.9m));
var identityC = new Identity("alice@thecompetition.com", new FacialFeatures("blue", 0.9m));
Authenticator.AreSameObject(identityB, identityC));
// => false
```
