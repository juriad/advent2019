<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE wrapper [
	<!ENTITY input SYSTEM "input">
]>
<xsl:stylesheet 
	version="1.1" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
	xmlns:exslt="http://exslt.org/common"
	xmlns:xs="http://www.w3.org/2001/XMLSchema" 
	xmlns:str="http://exslt.org/strings"
	exclude-result-prefixes="xs">
	<xsl:output method="xml" omit-xml-declaration="yes" />

	<xsl:template name="split">
		<xsl:param name="text"/>
		<xsl:param name="num"/>

		<xsl:if test="string-length($text)">
			<xsl:element name="{$num}">
				<xsl:if test="$num='i0'">
					<xsl:attribute name="op"/>
				</xsl:if>
				<xsl:value-of select="substring-before(concat($text,','),',')"/>
			</xsl:element>
			<xsl:call-template name="split">
				<xsl:with-param name="text" select="substring-after($text, ',')"/>
				<xsl:with-param name="num" select="concat('i', substring($num, 2) + 1)" /> 
			</xsl:call-template>
		</xsl:if>
	</xsl:template>

	<xsl:template name="step">
		<xsl:param name="mem"/>

		<xsl:variable name="iop" select="count(exslt:node-set($mem)/*[@op]/preceding::*)"/>
		<xsl:variable name="op" select="exslt:node-set($mem)/*[local-name(.) = concat('i', $iop + 0)]"/>

		<xsl:variable name="arg1" select="exslt:node-set($mem)/*[local-name(.) = concat('i', $iop + 1)]"/>
		<xsl:variable name="val1" select="exslt:node-set($mem)/*[local-name(.) = concat('i', $arg1)]"/>
		<xsl:variable name="arg2" select="exslt:node-set($mem)/*[local-name(.) = concat('i', $iop + 2)]"/>
		<xsl:variable name="val2" select="exslt:node-set($mem)/*[local-name(.) = concat('i', $arg2)]"/>
		<xsl:variable name="res" select="exslt:node-set($mem)/*[local-name(.) = concat('i', $iop + 3)]"/>

		<xsl:for-each select="exslt:node-set($mem)/*">
			<xsl:element name="{local-name()}">
				<xsl:if test="position() = $iop + 1 + 4">
					<xsl:attribute name="op"/>
				</xsl:if>

				<xsl:choose>
					<xsl:when test="position() - 1 = $res or $op = 99">
						<xsl:choose>
							<xsl:when test="$op = 1">
								<xsl:value-of select="$val1 + $val2"/>
							</xsl:when>
							<xsl:when test="$op = 2">
								<xsl:value-of select="$val1 * $val2"/>
							</xsl:when>
							<xsl:when test="$op = 99">
								<xsl:attribute name="halt" />
								<xsl:value-of select="." />
							</xsl:when>
							<xsl:otherwise>
								<xsl:message terminate="yes">
									<xsl:text>
									Illegal instruction
									</xsl:text>
									<xsl:value-of select="$op"/>
									<xsl:text>
									on position
									</xsl:text>
									<xsl:value-of select="$iop"/>
								</xsl:message>
							</xsl:otherwise>
						</xsl:choose>
					</xsl:when>
					<xsl:otherwise>
						<xsl:value-of select="."/>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:element>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="loop">
		<xsl:param name="mem"/>
		<xsl:param name="depth"/>

		<xsl:if test="$depth &gt; 0">
			<xsl:variable name="mem2">
				<xsl:call-template name="step">
					<xsl:with-param name="mem" select="$mem" />
				</xsl:call-template>
			</xsl:variable>

			<xsl:choose>
				<xsl:when test="exslt:node-set($mem2)/*[@halt]">
					<xsl:value-of select="exslt:node-set($mem2)/i0"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:call-template name="loop">
						<xsl:with-param name="mem" select="$mem2" />
						<xsl:with-param name="depth" select="$depth + 1" />
					</xsl:call-template>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:if>
	</xsl:template>

	<xsl:template name="setter">
		<xsl:param name="mem"/>
		<xsl:param name="noun"/>
		<xsl:param name="verb"/>

		<xsl:for-each select="exslt:node-set($mem)/*">
			<xsl:choose>
				<xsl:when test="local-name() = 'i1'">
					<i1><xsl:value-of select="$noun" /></i1>
				</xsl:when>
				<xsl:when test="local-name() = 'i2'">
					<i2><xsl:value-of select="$verb" /></i2>
				</xsl:when>
				<xsl:otherwise>
					<xsl:copy-of select="."/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="part1">
		<xsl:param name="mem"/>

		<xsl:variable name="mem1">
			<xsl:call-template name="setter">
				<xsl:with-param name="mem" select="$mem" />
				<xsl:with-param name="noun" select="12" />
				<xsl:with-param name="verb" select="2" />
			</xsl:call-template>
		</xsl:variable>

		<xsl:variable name="result">
			<xsl:call-template name="loop">
				<xsl:with-param name="mem" select="$mem1" />
				<xsl:with-param name="depth" select="1" />
			</xsl:call-template>
		</xsl:variable>
		<xsl:text>Part1: </xsl:text>
		<xsl:value-of select="$result"/>
		<xsl:text>
</xsl:text>
	</xsl:template>

	<xsl:template name="part2">
		<xsl:param name="mem"/>

		<xsl:for-each select="str:split(str:padding(100*100, ' '), '')">
			<xsl:variable name="value" select="position() - 1" />
			<xsl:variable name="mem1">
				<xsl:call-template name="setter">
					<xsl:with-param name="mem" select="$mem" />
					<xsl:with-param name="noun" select="($value - $value mod 100) div 100" />
					<xsl:with-param name="verb" select="$value mod 100" />
				</xsl:call-template>
			</xsl:variable>

			<xsl:variable name="result">
				<xsl:call-template name="loop">
					<xsl:with-param name="mem" select="$mem1" />
					<xsl:with-param name="depth" select="1" />
				</xsl:call-template>
			</xsl:variable>

<xsl:message>
	<xsl:text>Part2 (</xsl:text>
	<xsl:value-of select="$value"/>
	<xsl:text>): </xsl:text>
	<xsl:value-of select="$result"/>
	<xsl:text>
</xsl:text>
</xsl:message>

			<xsl:choose>
				<xsl:when test="$value &lt; 10000 and $result != '19690720'">
				</xsl:when>
				<xsl:otherwise>
					<xsl:text>Part2 (</xsl:text>
					<xsl:value-of select="$value"/>
					<xsl:text>): </xsl:text>
					<xsl:value-of select="$result"/>
					<xsl:text>
</xsl:text>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:for-each>
	</xsl:template>

	<xsl:template match="/">
		<xsl:variable name="input">
			&input;
		</xsl:variable>
		<xsl:variable name="mem">
			<xsl:call-template name="split">
				<xsl:with-param name="text" select="normalize-space($input)" />
				<xsl:with-param name="num" select="'i0'" />
			</xsl:call-template>
		</xsl:variable>

		<xsl:call-template name="part1">
			<xsl:with-param name="mem" select="$mem" />
		</xsl:call-template>

		<xsl:call-template name="part2">
			<xsl:with-param name="mem" select="$mem" />
		</xsl:call-template>
	</xsl:template>
</xsl:stylesheet>
