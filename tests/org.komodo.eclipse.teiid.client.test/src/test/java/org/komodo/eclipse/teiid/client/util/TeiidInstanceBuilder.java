/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.eclipse.teiid.client.util;

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
