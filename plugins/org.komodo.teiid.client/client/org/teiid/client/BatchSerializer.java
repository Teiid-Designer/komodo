/*************************************************************************************
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
 ************************************************************************************/
package org.teiid.client;

import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.List;

import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.Version;
import org.teiid.client.batch.Batch0Serializer;
import org.teiid.client.batch.Batch1Serializer;
import org.teiid.client.batch.Batch2Serializer;
import org.teiid.client.batch.Batch3Serializer;
import org.teiid.core.types.DataTypeManagerService;

/**
 *
 *
 * <ul>
 * <li>version 0: starts with 7.1 and uses simple serialization too broadly
 * <li>version 1: starts with 8.0 uses better string, blob, clob, xml, etc.
 *   add varbinary support.
 *   however was possibly silently truncating date/time values that were
 *   outside of jdbc allowed values
 * <li>version 2: starts with 8.2 and adds better array serialization and
 *   uses a safer date/time serialization
 * </ul>
 */
public abstract class BatchSerializer {

    private final ITeiidVersion teiidVersion;

    /**
     * @param teiidVersion
     */
    public BatchSerializer(ITeiidVersion teiidVersion) {
        this.teiidVersion = teiidVersion;
    }

    protected ITeiidVersion getTeiidVersion() {
        return this.teiidVersion;
    }

    protected DataTypeManagerService getDataTypeManager() {
        return DataTypeManagerService.getInstance(getTeiidVersion());
    }

    /**
     * @param teiidVersion
     * @return correct version of batch serializer according to teiid version
     */
    public static BatchSerializer getInstance(ITeiidVersion teiidVersion) {
        if (teiidVersion.isLessThan(Version.TEIID_8_0.get()))
            return new Batch0Serializer(teiidVersion);
        else if (teiidVersion.isLessThan(Version.TEIID_8_2.get()))
            return new Batch1Serializer(teiidVersion);
        else if (teiidVersion.isLessThan(Version.TEIID_8_6.get()))
            return new Batch2Serializer(teiidVersion);
        else
            return new Batch3Serializer(teiidVersion);
    }

    /**
     * @param in
     * @param types
     * @return batch of results
     * @throws IOException 
     * @throws ClassNotFoundException 
     */
    public abstract List<List<Object>> readBatch(ObjectInput in, String[] types) throws IOException, ClassNotFoundException;

    /**
     * @param out
     * @param types
     * @param batch
     * @throws IOException
     */
    public abstract void writeBatch(ObjectOutput out, String[] types, List<? extends List<?>> batch) throws IOException;

    /**
     * @param out
     * @param types
     * @param batch
     * @param version
     * @throws IOException
     */
    public abstract void writeBatch(ObjectOutput out, String[] types, List<? extends List<?>> batch, byte version) throws IOException;
}
