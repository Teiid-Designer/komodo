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
package org.komodo.modeshape.teiid.sequencer.v86;

import org.komodo.modeshape.teiid.sequencer.v85.TestTeiid85SqlSequencer;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;

/**
 *
 */
public class TestTeiid86SqlSequencer extends TestTeiid85SqlSequencer {

    /**
     */
    public TestTeiid86SqlSequencer() {
        super(Version.TEIID_8_6.get());
    }

    /**
     * @param teiidVersion
     */
    protected TestTeiid86SqlSequencer(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

}
