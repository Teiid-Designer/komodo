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
package org.komodo.teiid.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.teiid.adminapi.PropertyDefinition;
import org.teiid.adminapi.Translator;
import org.teiid.adminapi.VDB;

public class TeiidArtifactFactory {

    /**
     * The prefix used before the workspace identifier when creating a Preview VDB name.
     */
    public static final String PREVIEW_PREFIX = "PREVIEW_"; //$NON-NLS-1$

    public TeiidDataSource createDataSource(String name, Properties dataSource) {

        TeiidDataSource teiidDataSource = new TeiidDataSourceImpl(name, dataSource); //$NON-NLS-1$

        return teiidDataSource;
    }

    public TeiidTranslator createTranslator(Translator translator) {
        TeiidTranslatorImpl teiidTranslator = new TeiidTranslatorImpl(translator);
        return teiidTranslator;
    }

    public TeiidVdb createVdb(VDB vdb) throws Exception {
        TeiidVdb teiidVdb = new TeiidVdbImpl(vdb);
        return teiidVdb;
    }

    @SuppressWarnings( "unchecked" )
    public TeiidPropertyDefinition createPropertyDefinition(PropertyDefinition propDef) {
        TeiidPropertyDefinition teiidPropDef = new TeiidPropertyDefinition();

        Collection<Object> allowedValues = propDef.getAllowedValues();
        List<String> av = new ArrayList<>();
        for(Object value : allowedValues)
            av.add(value.toString());
        teiidPropDef.setAllowedValues(av);

        teiidPropDef.setCategory(propDef.getCategory());
        teiidPropDef.setDefaultValue(propDef.getDefaultValue());
        teiidPropDef.setDescription(propDef.getDescription());
        teiidPropDef.setDisplayName(propDef.getDisplayName());
        teiidPropDef.setName(propDef.getName());
        teiidPropDef.setProperties(propDef.getProperties());
        teiidPropDef.setPropertyTypeClassName(propDef.getPropertyTypeClassName());

        teiidPropDef.setAdvanced(propDef.isAdvanced());
        teiidPropDef.setConstrainedToAllowedValues(propDef.isConstrainedToAllowedValues());
        teiidPropDef.setMasked(propDef.isMasked());
        teiidPropDef.setModifiable(propDef.isModifiable());
        teiidPropDef.setRequired(propDef.isRequired());

        if (propDef.getRequiresRestart() != null) {
            TeiidPropertyDefinition.RestartType restartType = TeiidPropertyDefinition.RestartType.findRestartType(propDef.getRequiresRestart().name());
            teiidPropDef.setRequiresRestart(restartType);
        }

        return teiidPropDef;
    }

}
