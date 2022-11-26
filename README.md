## URIStringBuilder
Library to assist the creation of strings in a URI format.


PS: This library can't be used to parse uri strings and determine whether the format is valid or not. 
It is just for creating uri formatted strings.

## Motivation
Sometimes we need to build URLs out of variables. For example, say you have 
a variable that represents the path portion, and another variable that represents query
strings:
```java
final String fullUrl = "http://foo.bar/" + path + query;
```
The following concerns arise from the above operation: 
- What if the variable *path* already comes with a slash at the start?
- What if path contains a trailing slash, such that, when concatenated with the query string,
it may end up with something like */?*
- What if query doesn't come with the question tag at the start? 

It may be cumbersome to think all possibility that can arise and address them all. You
likely will end up with a boiler plate code and will lose focus on your original task.

This lib will assist you with the appending of different parts of a URL and end up with a concise format, 
getting rid of any possible trailing slashes, question tags, among other things. Just bear in mind this is **NOT** a validation lib.

With the use of this lib, the code above can adapted to something like this: 
```java
// With RawUriStringBuilder
final String urlUsingRawBuilder = new RawUriStringBuilder("http://foo.bar/")
        .append(path)
        .append("?")
        .append(query)
        .build();

// With StructuredUriStringBuilder
final String urlUsingStructuredBuilder = new StructuredUriStringBuilder("http://foo.bar/")
        .appendPath(path)
        .appendQuery(query)
        .build();
```
----
## Examples

Concatenating different portions of the path in different appends invocation:
```java
// With RawUriStringBuilder
final String urlUsingRawBuilder = new RawUriStringBuilder("http://foo.bar/")
        .append(pathPortion1)
        .append(pathPortion2)
        .append("?")
        .append(query)
        .build();

// With StructuredUriStringBuilder
final String urlUsingStructuredBuilder = new StructuredUriStringBuilder("http://foo.bar/")
        .appendPath(pathPortion1)
        .appendPath(pathPortion2)
        .appendQuery(query)
        .build();
```

Not only path portions can be concatenated in different appends, but also the port, query strings, scheme, host portions: 
```java
// With StructuredUriStringBuilder
final String fullUrl = new StructuredUriStringBuilder()
        .appendScheme("ht")
        .appendScheme("tp://")
        .appendHost("www")
        .appendHost(".foo.bar")
        .appendPath("api") // /api/
        .appendPath("v1") // /api/v1
        .appendQuery("ticket", "123456") // ?ticket=123456
        .appendQuery("processed") // ?ticket=123456&processed
        .appendQuery("user", "1") // ?ticket=123456&processed&user=1
        .build();

// After build:
// http://www.foo.bar/api/v1?ticket=123456&processed&processed&user=1
   
```

The appends can come with trailing comas or query parts and they won't duplicate: 
```java
// With StructuredUriStringBuilder
final String fullUrl = new StructuredUriStringBuilder()
        .appendScheme("ht")
        .appendScheme("tp://")
        .appendHost("www")
        .appendHost(".foo.bar/")
        .appendPath("/api/") 
        .appendPath("/v1/") 
        .appendQuery("?ticket", "123456") 
        .appendQuery("processed") 
        .appendQuery("user", "1") 
        .build();

// After build:
// http://www.foo.bar/api/v1?ticket=123456&processed&processed&user=1
```


And it doesn't matter the order of the invocation: 
```java
// With StructuredUriStringBuilder
final String fullUrl = new StructuredUriStringBuilder()
        .appendQuery("ticket", "123456") 
        .appendScheme("tp")
        .appendHost("www")
        .appendHost(".foo.bar")
        .appendPath("api") 
        .appendScheme("ht")
        .appendPath("v1") 
        .appendQuery("processed") 
        .appendQuery("user", "1") 
        .build();
// After build:
// http://www.foo.bar/api/v1?ticket=123456&processed&processed&user=1
```
## To install
This library is not available in the maven central for now, therefore you will need to clone the repo:
```shell
git clone https://github.com/marcelus20/URIStringBuilder
```
Cd into the folder:
```shell
cd URIStringBuilder
```

Issue the *maven clean install* command:
```shell
mvn clean install
```
**IMPORTANT:**  Build project from the parent POM, as it will build the dependencies submodule in the correct sequence. 

After doing the above steps, you will be able to import this lib into your project:

### StructuredUriStringBuilder
```xml
    <dependency>
        <groupId>com.marcelus.uristringbuilder.structuredstring</groupId>
        <artifactId>structured-uri-string-builder</artifactId>
        <version>1.0.0-SNAPSHOT</version>
    </dependency>
```

### RawUriStringBuilder
```xml
    <dependency>
        <groupId>com.marcelus.uristringbuilder.rawstring</groupId>
        <artifactId>raw-uri-string-builder</artifactId>
        <version>1.0.0-SNAPSHOT</version>
    </dependency>
```
PS: To see what releases are available, check the [tags](https://github.com/marcelus20/URIStringBuilder/tags) page.

---
## FAQ

#### 1 - What's the difference between RawUriStringBuilder and the StructuredUriStringBuilder?

R: RawUriStringBuilder **doesn't have knowledge** of what portion of the url the *append*
method is taking, but it will do **its best** to **guess** and place the parts in their correct location.

For example, The *RawUriStringBuilder* doesn't know how to delimiter the end of the host portion and the start
of the path portion, if the slash is not specified in either these places:

1) At the end of the host portion
```java
    .append("endHost/").append("startPath")
```

2) Or at the start of the first path portion 
```java
    .append("endHost").append("/startPath")
```
3) Or between the end of the host and the start of the path portions
```java
    .append("endHost").append("/").append("startPath")
```

If the slash is not specified in either these three locations, the RawUriStringBuilder will
treat the *"startPath"* as part of the host, and this doesn't happen to the StructuredUriStringBuilder
since it knows what is the host and what is the path portions thanks to their *appendHost* and *appendPath* dedicated methods:

```java
// using the StructuredUriStringBuilder:
    .appendHost("endHost").appendPath("startPath")
```
the StructuredUriStringBuilder will know that the slash must be placed between *"endHost"* and *"startPath"*. The same
applies for the end of the path and the start of a query string. The  RawUriStringBuilder doesn't know how to delimiter it if the question tag 
isn't specified:

1) At the end of the path or host portion
```java
    .append("endHost?").append("key=value")
```

2) Or at the start of the first key/value portion
```java
    .append("endHost").append("?key=value")
```
3) Or between the end of the host and the start of the path portions
```java
    .append("endHost").append("?").append("key=value")
```

Another difference is that, RawUriStringBuilder will default the scheme protocol to https if it's ommited or not 
present in the first append or constructor whereas the StructuredUriStringBuilder will generate a string uri without the
scheme:

```java
// using the RawUriStringBuilder:
    new RawUriStringBuilder("foo.bar.fizz/").append("/buzz").build();
// Outcome will be "https://foo.bar.fizz/buzz"

// using the StructuredUriStringBuilder:
    new RawUriStringBuilder("foo.bar.fizz/").appendPath("/buzz").build();
// Outcome will be "foo.bar.fizz/buzz"
```

#### 2 - How does the RawUriStringBuilder know if string portion is a scheme protocol to default it to https when omitted? 
R: One of the RawUriStringBuilder dependencies, that is also a submodule of this project,  is the [com.marcelus.uristringbuilder.available.uri.schemes.URISchemes](available-uri-schemes/src/main/java/com/marcelus/uristringbuilder/available/uri/schemes/URISchemes.java) 
enum a which contains the schemes listed in this [Wikipedia page](https://en.wikipedia.org/wiki/List_of_URI_schemes).

It makes use of the helper static method isScheme(String s) that detects when the portion is a scheme. 

##### 3 - When to use RawUriStringBuilder and when to use StructuredUriStringBuilder?

R: It depends on the use case. If you don't have control of how each portion of the URI will be given to you, like, 
trailing slashes, question tags, etc, and you are very sensitive to malformed URIs, you are better off using the 
**StructuredUriStringBuilder** as it will put each portion in its correct place and properly link one another. 

Now, if you arealy have the portions well formed, and you just want to concatenate them without worrying about trailing slashes, 
query components, you can use the RawUriStringBuilder instead. 

#### 4 - What about the other modules of this project, are they of any use?

R: They are just dependencies for the RawUriStringBuilder and StructuredUriStringBuilder, nothing else. 
They will need to be built along though, thus when building this project, make sure to build
from the parent root pom, and not each module separately. 

---
### Contributions
Feel free to contribute, just please consider the following:
- The methods of the builder should never throw, return an empty string instead using java.util.Optional for handling null
or bad parameters. 
- Make all parameters of methods final, use immutable code as much as possible. If you must create setters, use immutable 
setters (withers for example) that returns a new object as opposed to changing the field.
- URIStringBuilder software is distributed under the GNU General Public open source license, and your contributions will 
be licensed in the same way.
- Open Pull Requests for the added features with a satisfactory test coverage (60% at least).
- Create an issue if you want to discuss a new feature.

## License
[GNU GENERAL PUBLIC LICENSE](LICENSE)
