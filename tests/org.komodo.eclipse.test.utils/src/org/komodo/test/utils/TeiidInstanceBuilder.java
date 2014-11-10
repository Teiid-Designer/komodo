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
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.version.ITeiidVersion;

@SuppressWarnings( {"javadoc"} )
public class TeiidInstanceBuilder {

	private TeiidInstance teiidInstance;

	public TeiidInstanceBuilder(ITeiidVersion teiidVersion) throws Exception {
		teiidInstance = mock(TeiidInstance.class);
		when(teiidInstance.getVersion()).thenReturn(teiidVersion);

		TeiidAdminInfo adminInfo = mock(TeiidAdminInfo.class);
		when(teiidInstance.getTeiidAdminInfo()).thenReturn(adminInfo);

		TeiidJdbcInfo jdbcInfo = mock(TeiidJdbcInfo.class);
		when(teiidInstance.getTeiidJdbcInfo()).thenReturn(jdbcInfo);
	}

	/**
	 * @return the teiidInstance
	 */
	public TeiidInstance getTeiidInstance() {
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
		when(teiidInstance.getTeiidAdminInfo().getPort()).thenReturn(port);
	}

}
