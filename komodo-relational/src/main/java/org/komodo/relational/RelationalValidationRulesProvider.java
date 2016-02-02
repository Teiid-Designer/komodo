/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import org.komodo.shell.AbstractValidationRulesProvider;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository;


/**
 * A validation rules provider for relational objects.
 */
public class RelationalValidationRulesProvider extends AbstractValidationRulesProvider {

    private static final String VALIDATION_RULES_FILE_NAME = "relationalValidationRulesDefault.xml"; //$NON-NLS-1$

    /**
     * Constructs a rules provider for relational objects.
     */
    public RelationalValidationRulesProvider() {
        // nothing to do
    }

    @Override
    public void importRules( final Repository repo, final Repository.UnitOfWork uow, final boolean overwriteExisting ) throws KException {
        // Get the File
        //File rulesXmlFile = new File( getClass().getClassLoader().getResource(VALIDATION_RULES_FILE_NAME).getFile() );
        InputStream rulesStream = getClass().getClassLoader().getResourceAsStream(VALIDATION_RULES_FILE_NAME);
        File tempRulesFile;
        try {
            tempRulesFile = File.createTempFile("rulesFile", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
            Files.copy(rulesStream, tempRulesFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException ex) {
            throw new KException(ex);
        }
        tempRulesFile.deleteOnExit();

        repo.getValidationManager().validateRules(tempRulesFile);
        repo.getValidationManager().importRules(tempRulesFile, uow, overwriteExisting);
    }

}
