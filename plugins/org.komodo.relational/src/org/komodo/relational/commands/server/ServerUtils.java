/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.server;

import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;

/**
 * Common methods used by server commands
 */
public class ServerUtils {
    
    /**
     * Get the teiid object with the supplied name from the workspace
     * @param wsMgr the workspace manager
     * @param wsStatus the workspace status
     * @param serverName the name of the teiid object
     * @return the teiid object
     * @throws KException the exception
     */
    public static Teiid getWorkspaceTeiidObject(WorkspaceManager wsMgr, WorkspaceStatus wsStatus, String serverName) throws KException {
        Teiid resultTeiid = null;
        List<Teiid> teiids = wsMgr.findTeiids(wsStatus.getTransaction());

        if (teiids == null || teiids.size() == 0) {
            return resultTeiid;
        }

        for (Teiid theTeiid : teiids) {
            String teiidName = theTeiid.getName(wsStatus.getTransaction());
            if (serverName.equals(theTeiid.getId(wsStatus.getTransaction())) || serverName.equals(teiidName)) {
                resultTeiid = theTeiid;
                break;
            }
        }
        return resultTeiid;
    }

}
