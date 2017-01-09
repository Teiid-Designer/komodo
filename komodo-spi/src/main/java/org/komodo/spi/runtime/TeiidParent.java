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
package org.komodo.spi.runtime;

import org.komodo.spi.runtime.version.TeiidVersion;

/**
 * The parent teiid instance of a Teiid instance
 */
public interface TeiidParent extends HostProvider {

    /**
     * @return actual parent object
     */
    Object getParentObject();

    /**
     * @return unique id of this parent
     */
    String getId();

    /**
     * @return name of this parent
     */
    String getName();

    /**
     * @return the child teiid instance of this parent
     */
    TeiidInstance getTeiidInstance(TeiidVersion teiidVersion);

    /**
     * @return port
     */
    int getPort();

    /**
     * @return user name used for connections
     */
    String getUsername();

    /**
     * @return password used for connections
     */
    String getPassword();

    /**
     * @return whether connection has been secured
     */
    boolean isSecure();

    /**
     * @return event manager which will be notified of changes to the child {@link TeiidInstance}
     */
    EventManager getEventManager();

    /**
     * @return whether this parent is valid
     */
    boolean isSound();

}
