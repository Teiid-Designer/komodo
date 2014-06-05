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
package org.teiid.runtime.client.driver.provider;

import org.eclipse.datatools.connectivity.drivers.DefaultDriverValuesProvider;
import org.eclipse.datatools.connectivity.drivers.IDriverValuesProvider;
import org.teiid.runtime.client.TeiidRuntimePlugin;

/**
 * Provides values for the default instance of the Teiid Connection Driver profile
 */
public class TeiidDriverValuesProvider extends DefaultDriverValuesProvider {

    @Override
    public String createDefaultValue(String key) {
        if (key.equals(IDriverValuesProvider.VALUE_CREATE_DEFAULT))
            return Boolean.TRUE.toString();

        if (key.equals(IDriverValuesProvider.VALUE_JARLIST)) {
            return TeiidRuntimePlugin.getPluginPath();
        }

        return super.createDefaultValue(key);
    }
}
