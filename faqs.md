---
title: "FAQs"
layout: default
weight: 4
group: doc
---

FAQs
===

If your concerns are not addressed by any of the following, please email us at discuss@factorie.cs.umass.edu.


### My Maven build is failing because it cannot retrieve dependencies from the IESL repository?

In other words, if your error looks like the following:

    ...
    [ERROR] Failed to execute goal on project factorie: Could not resolve dependencies for project cc.factorie:factorie:jar:XXX: Failed to collect dependencies for [...]:
    Failed to read artifact descriptor for bibtex:bibtex:jar:20040801: Could not transfer artifact bibtex:bibtex:pom:20040801 from/to dev-iesl.cs.umass.edu (https://dev-iesl.cs.umass.edu/nexus/content/repositories/thirdparty/): Error transferring file: sun.security.validator.ValidatorException: PKIX path validation failed: java.security.cert.CertPathValidatorException: Path does not chain with any of the trust anchors -> [Help 1]
    org.apache.maven.lifecycle.LifecycleExecutionException: Failed to execute goal on project factorie: Could not resolve dependencies for project cc.factorie:factorie:jar:1.0-SNAPSHOT: Failed to collect dependencies for [...]
        at org.apache.maven.lifecycle.internal.LifecycleDependencyResolver.getDependencies(LifecycleDependencyResolver.java:196)
        at org.apache.maven.lifecycle.internal.LifecycleDependencyResolver.resolveProjectDependencies(LifecycleDependencyResolver.java:108)
        at org.apache.maven.lifecycle.internal.MojoExecutor.ensureDependenciesAreResolved(MojoExecutor.java:258)
    ...

You probably have some SSL certificate issues. Try the following steps (Thanks, Ben!):

1. load in Firefox: https://dev-iesl.cs.umass.edu/nexus/content/repositories/thirdparty/bibtex/bibtex/20040801/
2. go to Tools->"Page Info" then go the the tab "Security", click on "View Certificate" go to tab "Details" and click "Export..."
3. save the resulting certificate file somewhere on your computer
4. in a terminal, paste the following (some of the paths will be different for your machine):

        $ sudo /usr/lib/jvm/java-1.6.0-openjdk-1.6.0.0.x86_64/jre/bin/keytool -import -alias factorie_key_store-1 -file /path/to/certificate -keystore /etc/pki/java/cacerts
5. the default password for the keystore is "changeit" and to the question "Trust this certificate?" answer "yes"
For the points 4 to 5 above, use the Option 1 of http://stufftohelpyouout.blogspot.co.uk/2008/10/unable-to-find-valid-certification-path.html
 
If these step don't work, please email us at discuss@factorie.cs.umass.edu.
