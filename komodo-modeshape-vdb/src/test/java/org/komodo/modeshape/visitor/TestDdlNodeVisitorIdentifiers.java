/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.modeshape.visitor;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

import org.junit.Test;
import org.komodo.spi.runtime.version.TeiidVersionProvider;

@SuppressWarnings( { "javadoc", "nls" } )
public class TestDdlNodeVisitorIdentifiers {
	private DdlNodeVisitor visitor=new DdlNodeVisitor(TeiidVersionProvider.getInstance().getTeiidVersion(), true);

	@Test
	public void testEscapeOptionKeyNonQuoted(){
		assertThat("Non-quoted key should be quoted",visitor.escapeOptionKey("abc"), is("\"abc\""));
	}
	@Test
	public void testEscapeOptionKeyQuoted(){
		assertThat("Opening and ending pair of quotes should not be duplicated",visitor.escapeOptionKey("\"abc\""), is("\"abc\""));
	}
	@Test
	public void testEscapeOptionKeyEscapeQuotes(){
		assertThat("Double quotes should be escaped",visitor.escapeOptionKey("ab\"c"), is("\"ab\"\"c\""));
	}
	@Test
	public void testEscapeOptionKeyNoSplit(){
		assertThat("Each component of key should be quoted",visitor.escapeOptionKey("a.b.c"), is("\"a\".\"b\".\"c\""));
	}
	@Test
	public void testEscapeOptionKeyDots(){
		assertThat("Non-quoted key should be quoted",visitor.escapeOptionKey("abc"), is("\"abc\""));
	}
	@Test
	public void testEscapeOptionKeyEscapeAllQuotes(){
		assertThat("Double quotes should be escaped",visitor.escapeOptionKey("abc.abc.a\"c"), is("\"abc\".\"abc\".\"a\"\"c\""));
	}
	@Test
	public void testEscapeOptionKeyEscapeAllQuotes2(){
		assertThat("Double quotes should be escaped",visitor.escapeOptionKey("ab\"\"c.abc"), is("\"ab\"\"\"\"c\".\"abc\""));
	}
	@Test
	public void testEscapeOptionKeyQuotedOneChar(){
		assertThat("Non-quoted key should be quoted",visitor.escapeOptionKey("a"), is("\"a\""));
	}
	@Test
	public void testEscapeOptionKeyNonQuotedOneChar(){
		assertThat("Non-quoted key should be quoted",visitor.escapeOptionKey("\"a\""), is("\"a\""));
	}

	@Test
	public void testEscapeSinglePartNoQuote(){
		assertThat("Identifiers should not be quoted by default",visitor.escapeSinglePart("abc"), is("abc"));
	}
	@Test
	public void testEscapeSinglePartHash(){
		assertThat("Identifier can start with @ or #",visitor.escapeSinglePart("#abc"), is("#abc"));
	}
	@Test
	public void testEscapeSinglePartAtSign(){
		assertThat("Identifier can start with @ or #",visitor.escapeSinglePart("@abc"), is("@abc"));
	}
	@Test
	public void testEscapeSinglePartTwoHashes(){
		assertThat("Contains special characters.It should be quoted",visitor.escapeSinglePart("##abc"), is("\"##abc\""));
	}
	@Test
	public void testEscapeSinglePartDigit(){
		assertThat("Identifiers should not be quoted by default",visitor.escapeSinglePart("abc1"), is("abc1"));
	}
	@Test
	public void testEscapeSinglePartUnderScore(){
		assertThat("Identifier can cotain '_'",visitor.escapeSinglePart("a_bc"), is("a_bc"));
	}
	@Test
	public void testEscapeSinglePartReserved(){
		assertThat("Reserved keyword. Must be quoted",visitor.escapeSinglePart("OPTIONS"), is("\"OPTIONS\""));
	}
	@Test
	public void testEscapeSinglePartSpecialChar(){
		assertThat("Special character. Must be quoted",visitor.escapeSinglePart("a&bc"), is("\"a&bc\""));
	}
}
