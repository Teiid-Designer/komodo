package org.teiid.runtime.client.admin;

import java.util.ArrayList;
import java.util.Collection;
import java.util.UUID;

import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.uuid.WorkspaceUUIDService;


/**
 *
 */
public class ModelConnectionMatcher {

    /**
     * The prefix used before the workspace identifier when creating a Preview VDB name.
     */
    public static final String PREVIEW_PREFIX = "PREVIEW_"; //$NON-NLS-1$
    
    public Collection<TeiidDataSource> findTeiidDataSources( Collection<String> names) throws Exception {
        Collection<TeiidDataSource> dataSources = new ArrayList<TeiidDataSource>();

        for (String name : names) {
            if (name.equalsIgnoreCase("DefaultDS") || name.equalsIgnoreCase("JmsXA")) { //$NON-NLS-1$ //$NON-NLS-2$
                continue;
            }
            TCTeiidDataSource tds = new TCTeiidDataSource(name, name, "<unknown>"); //$NON-NLS-1$
            
            if (name.startsWith(PREVIEW_PREFIX)) {
                UUID workspaceUuid = WorkspaceUUIDService.getInstance().getUUID();
                if (name.length() > workspaceUuid.toString().length() + 8) {
                    tds.setPreview(true);
                }
            }
            dataSources.add(tds);
        }

        return dataSources;
    }
}
