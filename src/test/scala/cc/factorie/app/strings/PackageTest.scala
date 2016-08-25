package cc.factorie.app.strings

import org.junit.Test
import org.junit.Assert.assertEquals

class PackageTest {
  
  @Test def testSimplifyDigits(): Unit = {
		assertEquals("<YEAR>", simplifyDigits("2016"));
		assertEquals("<YEAR>", simplifyDigits("1984"));
		assertEquals("##a#", simplifyDigits("19a4"));
		assertEquals("###s", simplifyDigits("201s"));
	  assertEquals("<NUM>", simplifyDigits("20161234"));
	  assertEquals("<NUM>", simplifyDigits("010101"));
	  assertEquals("<NUM>", simplifyDigits("0"));
	  assertEquals("<NUM>", simplifyDigits("987654321"));
	  assertEquals("#########a", simplifyDigits("987654321a"));
	  assertEquals("####a", simplifyDigits("1984a"));
	  assertEquals("#a", simplifyDigits("0a"));
	  assertEquals("a#", simplifyDigits("a0"));
	  assertEquals("A-# St#ak Sauc#", simplifyDigits("A-1 St3ak Sauc3"));
  }
}