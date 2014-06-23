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
package org.komodo.test.utils;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.komodo.spi.runtime.ITeiidAdminInfo;
import org.komodo.spi.runtime.ITeiidInstance;
import org.komodo.spi.runtime.ITeiidJdbcInfo;
import org.komodo.spi.runtime.version.ITeiidVersion;

/**
 *
 */
public class TeiidInstanceBuilder {

	private ITeiidInstance teiidInstance;

	public TeiidInstanceBuilder(ITeiidVersion teiidVersion) {
		teiidInstance = mock(ITeiidInstance.class);
		when(teiidInstance.getVersion()).thenReturn(teiidVersion);

		ITeiidAdminInfo adminInfo = mock(ITeiidAdminInfo.class);
		when(teiidInstance.getTeiidAdminInfo()).thenReturn(adminInfo);

		ITeiidJdbcInfo jdbcInfo = mock(ITeiidJdbcInfo.class);
		when(teiidInstance.getTeiidJdbcInfo()).thenReturn(jdbcInfo);
	}

	/**
	 * @return the teiidInstance
	 */
	public ITeiidInstance getTeiidInstance() {
		return this.teiidInstance;
	}

	public void setHost(String host) {
		when(teiidInstance.getHost()).thenReturn(host);
	}

	public void setUserName(String userName) {
		when(teiidInstance.getTeiidAdminInfo().getUsername()).thenReturn(userName);
	}

	public void setPassword(String password) {
		when(teiidInstance.getTeiidAdminInfo().getPassword()).thenReturn(password);
	}

	public void setPort(int port) {
		when(teiidInstance.getTeiidAdminInfo().getPortNumber()).thenReturn(port);
	}

}
