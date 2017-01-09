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
import org.komodo.modeshape.teiid.sequencer.v8127.Test8127Sequencers;
import org.komodo.modeshape.teiid.sequencer.v8127.TestTeiid8127SqlSequencer;
import org.komodo.modeshape.teiid.sequencer.v911.Test911Sequencers;
import org.komodo.modeshape.teiid.sequencer.v911.TestTeiid911SqlSequencer;

@SuppressWarnings( "javadoc" )
@RunWith( Suite.class )
@Suite.SuiteClasses( {
                                        // sequencer
                                        TestTeiid8127SqlSequencer.class,

                                        // sequencers
                                        Test8127Sequencers.class,

                                        // sequencer
                                        TestTeiid911SqlSequencer.class,

                                        // sequencers
                                        Test911Sequencers.class,
                                    } )
public class AllTests {
    // nothing to do
}
