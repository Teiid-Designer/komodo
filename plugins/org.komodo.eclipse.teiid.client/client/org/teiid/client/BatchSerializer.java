/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
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
 * @since 4.2
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
