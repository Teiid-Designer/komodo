/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.table;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.table}.
 */
@SuppressWarnings( "javadoc" )
public final class TableCommandsI18n extends I18n {

    public static String addAccessPatternExamples;
    public static String addAccessPatternHelp;
    public static String addAccessPatternUsage;

    public static String addColumnExamples;
    public static String addColumnHelp;
    public static String addColumnUsage;

    public static String addForeignKeyExamples;
    public static String addForeignKeyHelp;
    public static String addForeignKeyUsage;

    public static String addIndexExamples;
    public static String addIndexHelp;
    public static String addIndexUsage;

    public static String addPrimaryKeyExamples;
    public static String addPrimaryKeyHelp;
    public static String addPrimaryKeyUsage;

    public static String addUniqueConstraintExamples;
    public static String addUniqueConstraintHelp;
    public static String addUniqueConstraintUsage;

    public static String deleteAccessPatternExamples;
    public static String deleteAccessPatternHelp;
    public static String deleteAccessPatternUsage;

    public static String deleteColumnExamples;
    public static String deleteColumnHelp;
    public static String deleteColumnUsage;

    public static String deleteForeignKeyExamples;
    public static String deleteForeignKeyHelp;
    public static String deleteForeignKeyUsage;

    public static String deleteIndexExamples;
    public static String deleteIndexHelp;
    public static String deleteIndexUsage;

    public static String deletePrimaryKeyExamples;
    public static String deletePrimaryKeyHelp;
    public static String deletePrimaryKeyUsage;

    public static String deleteUniqueConstraintExamples;
    public static String deleteUniqueConstraintHelp;
    public static String deleteUniqueConstraintUsage;

    public static String setTablePropertyExamples;
    public static String setTablePropertyHelp;
    public static String setTablePropertyUsage;

    public static String showAccessPatternsExamples;
    public static String showAccessPatternsHelp;
    public static String showAccessPatternsUsage;

    public static String showColumnsExamples;
    public static String showColumnsHelp;
    public static String showColumnsUsage;

    public static String showIndexesExamples;
    public static String showIndexesHelp;
    public static String showIndexesUsage;

    public static String showUniqueConstraintsExamples;
    public static String showUniqueConstraintsHelp;
    public static String showUniqueConstraintsUsage;

    public static String unsetTablePropertyExamples;
    public static String unsetTablePropertyHelp;
    public static String unsetTablePropertyUsage;

    public static String accessPatternAdded;
    public static String columnAdded;
    public static String foreignKeyAdded;
    public static String invalidTablePath;
    public static String accessPatternDeleted;
    public static String accessPatternsHeader;
    public static String columnDeleted;
    public static String columnsHeader;
    public static String foreignKeyDeleted;
    public static String indexAdded;
    public static String indexDeleted;
    public static String indexesHeader;
    public static String invalidOnCommitPropertyValue;
    public static String invalidSchemaElementTypePropertyValue;
    public static String invalidTemporaryTableTypePropertyValue;
    public static String missingAccessPatternName;
    public static String missingColumnName;
    public static String missingForeignKeyName;
    public static String missingForeignKeyTableRefPath;
    public static String missingIndexName;
    public static String missingPrimaryKeyName;
    public static String missingUniqueConstraintName;
    public static String noAccessPatterns;
    public static String noColumns;
    public static String noIndexes;
    public static String noPkToRemove;
    public static String noUniqueConstraints;
    public static String pkExistsCantAdd;
    public static String primaryKeyAdded;
    public static String primaryKeyDeleted;
    public static String uniqueConstraintAdded;
    public static String uniqueConstraintDeleted;
    public static String uniqueConstraintsHeader;

    static {
        final TableCommandsI18n i18n = new TableCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private TableCommandsI18n() {
        // nothing to do
    }

}
