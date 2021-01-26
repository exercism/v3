## Recommended Learning Resources

Welcome to learning ColdFusion (CFML)!  CFML is a dynamic and loosely typed JVM scripting language.  It has two different file types. `.cfm` files (ColdFusion markup)_ are purely procedural and use the tag-based syntax for templating out HTML.  You would use these for views if building a web site:

**index.cfm**

```xml
<cfoutput>
	<html>
		<h1>Welcome to my site, #FirstName# #LastName#!
	</html>
</cfoutput>
```

The other type of file is the `.cfc` (ColdFusion Component) and it is how you declare a class.  Components are used for business logic, models, and controllers.  These usually use the script syntax, which reads similar to JavaScript.

**Car.cfc**

```js
component accessors=true {
	property name='make' type='string';
	property name='model' type='string';
	property name='year' type='number';

	function init() {
		return this;
	}
 
}
```

Here are some guides for learning more about CFML
 
* [Modern ColdFusion (CFML) in 100 Minutes](https://www.gitbook.com/book/ortus/modern-coldfusion-cfml-in-100-minutes/details)
* [CF SCript Cheatsheet](http://www.petefreitag.com/cheatsheets/coldfusion/cfscript/)
* [Learn CF In A Week](http://www.learncfinaweek.com/)
* [CommandBox CLI Getting Started Guide](https://commandbox.ortusbooks.com/getting-started-guide)
