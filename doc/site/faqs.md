---
title: "FAQs"
layout: default
weight: 1
group: doc
---

FAQs
===

If your concerns are not addressed by any of the following, please email us at discuss@factorie.cs.umass.edu.

### How do I set the size of the Domain of a discrete variable?

Use the DomainSize annotation as follows.

```scala
@DomainSize(6) class DieRoll(initialFaceValue:Int) extends Discrete(initialFaceValue)
```

### How do I perform Gibbs/BlockGibbs Sampling for Undirected models?

See VariableSettingsSampler for Gibbs, and VariablesSettingsSampler for Block Gibbs (notice the "s" after Variable)

### How do I print the weights?

Here example code for TemplateWithDotStatistics2:

```scala
    def printTemplate2Weights[X<:DiscreteVectorVar,Y<:DiscreteVectorVar](template:TemplateWithDotStatistics2[X,Y]) {
      def name0(idx: Int) = template.statisticsDomains(0).dimensionName(idx)
      def name1(idx: Int) = template.statisticsDomains(1).dimensionName(idx)
      val dom0 = template.statisticsDomains(0).dimensionDomain
      val dom1 = template.statisticsDomains(1).dimensionDomain
      val namedWeights =
        for ((val0, idx0) <- dom0.values.zipWithIndex; (val1, idx1) <- dom1.values.zipWithIndex) yield {
          val w = template.weight(idx0, idx1)
          ((name0(val0.intValue), name1(val1.intValue)), w)
        }
      for (((cat0, cat1), w) <- namedWeights.sortBy(-_._2)) {
        println(cat0 + " " + cat1 + " " + w)
      }
    }
```

If you don't see names of CategoricalVectorDomains, add the following to that domain

```scala
override def dimensionName(idx:Int):String = {
  AffinityDomain.getCategory(idx).toString
}
```

### My Maven build is failing because it cannot retrieve dependencies from the IESL repository?

In other words, if your error looks like the following:

```
    ...
    [ERROR] Failed to execute goal on project factorie: Could not resolve dependencies for project cc.factorie:factorie:jar:XXX: Failed to collect dependencies for [...]:
    Failed to read artifact descriptor for bibtex:bibtex:jar:20040801: Could not transfer artifact bibtex:bibtex:pom:20040801 from/to dev-iesl.cs.umass.edu (https://dev-iesl.cs.umass.edu/nexus/content/repositories/thirdparty/): Error transferring file: sun.security.validator.ValidatorException: PKIX path validation failed: java.security.cert.CertPathValidatorException: Path does not chain with any of the trust anchors -> [Help 1]
    org.apache.maven.lifecycle.LifecycleExecutionException: Failed to execute goal on project factorie: Could not resolve dependencies for project cc.factorie:factorie:jar:1.0-SNAPSHOT: Failed to collect dependencies for [...]
        at org.apache.maven.lifecycle.internal.LifecycleDependencyResolver.getDependencies(LifecycleDependencyResolver.java:196)
        at org.apache.maven.lifecycle.internal.LifecycleDependencyResolver.resolveProjectDependencies(LifecycleDependencyResolver.java:108)
        at org.apache.maven.lifecycle.internal.MojoExecutor.ensureDependenciesAreResolved(MojoExecutor.java:258)
    ...
```

You probably have some SSL certificate issues. Try the following steps (Thanks, Ben!):

1. load in Firefox: https://dev-iesl.cs.umass.edu/nexus/content/repositories/thirdparty/bibtex/bibtex/20040801/
2. go to Tools->"Page Info" then go the the tab "Security", click on "View Certificate" go to tab "Details" and click "Export..."
3. save the resulting certificate file somewhere on your computer
4. in a terminal, paste the following (some of the paths will be different for your machine):

```
$ sudo /usr/lib/jvm/java-1.6.0-openjdk-1.6.0.0.x86_64/jre/bin/keytool -import -alias factorie_key_store-1 -file /path/to/certificate -keystore /etc/pki/java/cacerts
```

5. the default password for the keystore is "changeit" and to the question "Trust this certificate?" answer "yes"
For the points 4 to 5 above, use the Option 1 of http://stufftohelpyouout.blogspot.co.uk/2008/10/unable-to-find-valid-certification-path.html
 
If these step don't work, please email us at discuss@factorie.cs.umass.edu.
