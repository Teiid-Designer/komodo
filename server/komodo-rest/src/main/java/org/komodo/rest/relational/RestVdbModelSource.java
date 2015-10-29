/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import java.net.URI;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.ModelSource;
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
 * A VDB model source that can be used by GSON to build a JSON model source representation.
 */
public final class RestVdbModelSource extends KomodoRestEntity {

    /**
     * Label used to describe jndi name
     */
    public static final String JNDI_NAME_LABEL = KomodoService.encode(VdbLexicon.Source.JNDI_NAME);

    /**
     * Label used to describe translator
     */
    public static final String TRANSLATOR_LABEL = KomodoService.encode(VdbLexicon.Source.TRANSLATOR);

    private String jndiName;

    private String translator;

    /**
     * Constructor for use when deserializing
     */
    public RestVdbModelSource() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the vdb
     * @param id the id of this vdb
     * @param dataPath the data path of this vdb
     * @param kType the type of this vdb
     * @param hasChildren true if vdb has children
     * @param parentVdb the name of the parent vdb
     * @param parentModel the name of the parent model
     */
    public RestVdbModelSource(URI baseUri, String id, String dataPath, KomodoType kType,
                                                  boolean hasChildren, String parentVdb, String parentModel) {
        super(baseUri, id, dataPath, kType, hasChildren);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().buildVdbModelSourceUri(LinkType.SELF, parentVdb, parentModel, id)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().buildVdbModelSourceUri(LinkType.PARENT, parentVdb, parentModel, id)));
    }


    /**
     * @return the jndiName
     */
    public String getJndiName() {
        return this.jndiName;
    }

    /**
     * @param jndiName the jndiName to set
     */
    public void setJndiName(String jndiName) {
        this.jndiName = jndiName;
    }

    /**
     * @return the translator
     */
    public String getTranslator() {
        return this.translator;
    }

    /**
     * @param translator the translator to set
     */
    public void setTranslator(String translator) {
        this.translator = translator;
    }

    /**
     * @param source the source
     * @param model source model
     * @param parentVdb parent Vdb
     * @param baseUri base uri
     * @param uow the transaction
     * @return a new {@link RestVdbModelSource} based on the source {@link Model}
     * @throws KException if error occurs
     */
    public static RestVdbModelSource build(ModelSource source, final Model model, final Vdb parentVdb, final URI baseUri, final UnitOfWork uow) throws KException {

        final String vdbName = parentVdb.getName(uow);
        final String modelName = model.getName(uow);
        final RestVdbModelSource entity = new RestVdbModelSource(baseUri, source.getName(uow),
                                                                 source.getAbsolutePath(), source.getTypeIdentifier(uow),
                                                                 source.hasChildren(uow), vdbName, modelName);

        entity.setJndiName(source.getJndiName(uow));
        entity.setTranslator(source.getTranslatorName(uow));

        return entity;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.jndiName == null) ? 0 : this.jndiName.hashCode());
        result = prime * result + ((this.translator == null) ? 0 : this.translator.hashCode());
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
        RestVdbModelSource other = (RestVdbModelSource)obj;
        if (this.jndiName == null) {
            if (other.jndiName != null)
                return false;
        } else
            if (!this.jndiName.equals(other.jndiName))
                return false;
        if (this.translator == null) {
            if (other.translator != null)
                return false;
        } else
            if (!this.translator.equals(other.translator))
                return false;
        return true;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "RestVdbModelSource [jndiName=" + this.jndiName + ", translator=" + this.translator + ", id=" + this.id + ", dataPath=" + this.dataPath + ", kType=" + this.kType + ", hasChildren=" + this.hasChildren + ", properties=" + this.properties + ", links=" + this.links + "]";
    }
}
