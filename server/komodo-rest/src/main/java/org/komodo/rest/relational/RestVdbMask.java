/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import java.net.URI;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestEntity;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * A condition that can be used by GSON to build a JSON document representation.
 */
public final class RestVdbMask extends KomodoRestEntity {

    /**
     * Label used to describe name
     */
    public static final String NAME_LABEL = KomodoService.encode(VdbLexicon.DataRole.Permission.Mask.MASK);

    /**
     * Label used to describe order
     */
    public static final String ORDER_LABEL = KomodoService.encode(VdbLexicon.DataRole.Permission.Mask.ORDER);

    /**
     * An empty array of masks.
     */
    public static final RestVdbMask[] NO_MASKS = new RestVdbMask[ 0 ];

    private String name;

    private String order;

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbMask() {
        // nothing to do
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the vdb
     * @param id the id of this vdb
     * @param dataPath the data path of this vdb
     * @param kType the type of this vdb
     * @param hasChildren true if vdb has children
     * @param permission the name of the parent permission
     * @param dataRole the name of the parent data role
     * @param parentVdb the name of the parent vdb
     */
    public RestVdbMask(URI baseUri, String id, String dataPath, KomodoType kType, boolean hasChildren,
                                            String permission, String dataRole, String parentVdb) {
        super(baseUri, id, dataPath, kType, hasChildren);

        addLink(new RestLink(LinkType.SELF, getUriBuilder()
                             .buildVdbPermissionChildUri(LinkType.SELF, parentVdb, dataRole, permission, LinkType.MASKS, id)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder()
                             .buildVdbPermissionChildUri(LinkType.PARENT, parentVdb, dataRole, permission, LinkType.MASKS, id)));
    }

    /**
     * @return the name (can be empty)
     */
    public String getName() {
        return this.name;
    }

    /**
     * @param newName
     *        the new translator name (can be empty)
     */
    public void setName( final String newName ) {
        this.name = newName;
    }

    /**
     * @return the order
     */
    public String getOrder() {
        return this.order;
    }

    /**
     * @param order the order to set
     */
    public void setOrder(String order) {
        this.order = order;
    }

    /**
     * @param mask the source condition
     * @param permission parent permission
     * @param dataRole parent data role
     * @param parentVdb parent vdb
     * @param baseUri base uri
     * @param uow the transaction
     * @return a new {@link RestVdbMask} based on the source {@link Permission}
     * @throws KException if error occurs
     */
    public static RestVdbMask build(Mask mask, Permission permission, DataRole dataRole,
                                                                Vdb parentVdb, URI baseUri,
                                                                UnitOfWork uow) throws KException {
        final String vdbName = parentVdb.getName(uow);
        final String dataRoleName = dataRole.getName(uow);
        final String permissionName = permission.getName(uow);
        final RestVdbMask entity = new RestVdbMask(baseUri,
                                                                                       mask.getName(uow),
                                                                                       mask.getAbsolutePath(),
                                                                                       mask.getTypeIdentifier(uow),
                                                                                       mask.hasChildren(uow),
                                                                                       permissionName, dataRoleName, vdbName);

        entity.setName(mask.getName(uow));
        entity.setOrder(mask.getOrder(uow));

        return entity;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
        result = prime * result + ((this.order == null) ? 0 : this.order.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        RestVdbMask other = (RestVdbMask)obj;
        if (this.name == null) {
            if (other.name != null)
                return false;
        } else
            if (!this.name.equals(other.name))
                return false;
        if (this.order == null) {
            if (other.order != null)
                return false;
        } else
            if (!this.order.equals(other.order))
                return false;
        return true;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "RestVdbMask [name=" + this.name + ", order=" + this.order + ", id=" + this.id + ", dataPath=" + this.dataPath + ", kType=" + this.kType + ", hasChildren=" + this.hasChildren + ", properties=" + this.properties + ", links=" + this.links + "]";
    }
}
