/*
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
*/
package org.komodo.modeshape.teiid;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.komodo.modeshape.teiid.sql.v8.Test8Cloning;
import org.komodo.modeshape.teiid.sql.v8.Test8SQLStringVisitor;
import org.komodo.modeshape.teiid.sql.v8.TestQuery8Parser;
import org.komodo.modeshape.teiid.sql.v84.TestQuery84Parser;
import org.komodo.modeshape.teiid.sql.v85.Test85Cloning;
import org.komodo.modeshape.teiid.sql.v85.Test85SQLStringVisitor;
import org.komodo.modeshape.teiid.sql.v85.TestQuery85Parser;
import org.komodo.modeshape.teiid.sql.v86.Test86Cloning;
import org.komodo.modeshape.teiid.sql.v86.Test86SQLStringVisitor;
import org.komodo.modeshape.teiid.sql.v86.TestQuery86Parser;
import org.komodo.modeshape.teiid.sql.v87.Test87Cloning;
import org.komodo.modeshape.teiid.sql.v87.Test87SQLStringVisitor;
import org.komodo.modeshape.teiid.sql.v87.TestQuery87Parser;

@SuppressWarnings( "javadoc" )
@RunWith( Suite.class )
@Suite.SuiteClasses( {
                                        // query.sql
                                        Test8Cloning.class,
                                        TestQuery8Parser.class,
                                        TestQuery84Parser.class,
                                        Test8SQLStringVisitor.class,
                                        Test85Cloning.class,
                                        TestQuery85Parser.class,
                                        Test85SQLStringVisitor.class,
                                        Test86Cloning.class,
                                        TestQuery86Parser.class,
                                        Test86SQLStringVisitor.class,
                                        Test87Cloning.class,
                                        TestQuery87Parser.class,
                                        Test87SQLStringVisitor.class,
                                    } )
public class AllTests {
    // nothing to do
}
