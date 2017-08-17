
<xsl:stylesheet version="2.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema"
	xmlns:fn="http://www.w3.org/2005/xpath-functions" xmlns:xdt="http://www.w3.org/2005/xpath-datatypes"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:jb="urn:jboss:domain:1.4"
	exclude-result-prefixes="xs xsl xsi fn xdt jb cache-container">
	<xsl:output method="xml" version="1.0" encoding="UTF-8"
		indent="yes" />

	<xsl:template match="@*|node()">
		<xsl:copy>
			<xsl:apply-templates select="@*|node()" />
		</xsl:copy>
	</xsl:template>

	<xsl:template match="node()[name(.)='dependency']">
		<xsl:choose>
			<xsl:when test="groupId='aopalliance'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>Public Domain, per Creative Commons CC0</name>
							<url>https://creativecommons.org/publicdomain/zero/1.0/legalcode</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			<xsl:when test="groupId='avalon-framework'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>Apache License, Version 2.0</name>
							<url>https://svn.apache.org/repos/asf/avalon/site/framework/license.html</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			<xsl:when test="groupId='batik'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>Apache License, Version 2.0</name>
							<url>https://xmlgraphics.apache.org/batik/license.html</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			<xsl:when test="groupId='commons-configuration'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>Apache License, Version 2.0</name>
							<url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			<xsl:when test="groupId='com.twitter'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>Public Domain, per Creative Commons CC0</name>
							<url>https://creativecommons.org/publicdomain/zero/1.0/legalcode</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			<xsl:when test="groupId='com.google.jsinterop'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>Apache License, Version 2.0</name>
							<url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			<xsl:when test="groupId='com.github.virtuald'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>BSD License</name>
							<url>https://opensource.org/licenses/BSD-3-Clause</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			
			<xsl:when test="groupId='dom4j'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>DOM4J - BSD License</name>
							<url>https://raw.githubusercontent.com/dom4j/dom4j/dom4j_1_6_1/LICENSE.txt</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			
			<xsl:when test="groupId='javax.validation'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>Apache License, Version 2.0</name>
							<url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>

			<xsl:when test="groupId='jline'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>BSD</name>
							<url>http://jline.sourceforge.net/license.html</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			
			<!-- Can't find jsr94 source -->
			<xsl:when test="groupId='jsr94'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>Under BEA System</name>
                            <url>http://download.oracle.com/otndocs/jcp/jreng-1_0a-fr-spec-api-oth-JSpec/</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			<xsl:when test="groupId='org.apache.zookeeper'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>Apache License, Version 2.0</name>
							<url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			<xsl:when test="groupId='org.eclipse'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>Eclipse Public License - v 1.0</name>
							<url>http://www.eclipse.org/legal/epl-v10.html</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			<xsl:when test="groupId='org.eclipse.jgit'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>Eclipse Public License - v 1.0</name>
							<url>http://www.eclipse.org/legal/epl-v10.html</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			<xsl:when test="groupId='org.hdrhistogram'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>Public Domain, per Creative Commons CC0</name>
							<url>https://creativecommons.org/publicdomain/zero/1.0/legalcode</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			<xsl:when test="groupId='org.jdom'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
                            <name>Similar to Apache License but with the acknowledgment clause removed</name>
                            <url>https://raw.github.com/hunterhacker/jdom/master/LICENSE.txt</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			<xsl:when test="groupId='org.yaml'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>Apache License, Version 2.0</name>
							<url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
							<distribution>repo</distribution>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			<xsl:when test="groupId='xpp3'">
				<xsl:copy>
					<xsl:apply-templates select="@*|*[not(self::licenses)]" />
					<licenses>
						<license>
							<name>Indiana University Extreme! Lab Software License, vesion 1.1.1</name>
                            <url>https://raw.githubusercontent.com/aslom/xpp3/master/LICENSE.txt</url>
							<distribution>repo</distribution>
							<comments>The license applies to the Xpp3 classes (all classes below the org.xmlpull package with exception of classes directly in package org.xmlpull.v1 )</comments>
						</license>
						<license>
							<name>Public Domain, per Creative Commons CC0</name>
                            <url>https://creativecommons.org/publicdomain/zero/1.0/legalcode</url>
                            <distribution>repo</distribution>
							<comments>The license applies to the XmlPull API (all classes directly in the org.xmlpull.v1 package)</comments>
						</license>
					</licenses>
				</xsl:copy>
			</xsl:when>
			<xsl:otherwise>
				<xsl:copy>
					<xsl:apply-templates select="@*|node()" />
				</xsl:copy>
			</xsl:otherwise>

		</xsl:choose>
	</xsl:template>

</xsl:stylesheet>

